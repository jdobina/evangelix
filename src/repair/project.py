import copy
import os
from os.path import join, exists, relpath, basename, realpath
import shutil
import subprocess
import json
from utils import cd, diff
import logging
import tempfile
import sys
import re
import statistics
import time
from transformation import PrintfTransformer, BuggyTransformer, \
                           RepairedTransformer


logger = logging.getLogger(__name__)


class CompilationError(Exception):
    pass


class Project:

    def __init__(self, config, dir, buggy, build_cmd, configure_cmd):
        self.config = config
        if self.config['verbose']:
            self.subproc_output = sys.stderr
        else:
            self.subproc_output = subprocess.DEVNULL
        self.dir = dir
        self.buggy = join(self.dir, buggy)
        self.build_cmd = build_cmd
        self.configure_cmd = configure_cmd

        self.make_repairable = BuggyTransformer(self.config)
        self.tidy = RepairedTransformer(config)

    def initialize(self):
        self._orig_buggy = self.buggy + '.orig'
        shutil.copyfile(self.buggy, self._orig_buggy)

        if self.config['instr_printf'] is not None:
            self.configure()
            self.instrument_printf = PrintfTransformer(self.config)
            self.instrument_printf(self, self.config['instr_printf'])

        self._with_instr_buggy = self.buggy + '.with_instr'
        shutil.copyfile(self.buggy, self._with_instr_buggy)

        self._buggy_backup = self.buggy + '.backup'
        shutil.copyfile(self.buggy, self._buggy_backup)

    def restore_buggy(self):
        shutil.copyfile(self._buggy_backup, self.buggy)

    def restore_old_buggy(self):
        shutil.copyfile(self._old_buggy_backup, self._buggy_backup)
        shutil.copyfile(self._buggy_backup, self.buggy)

    def update_buggy(self):
        self._old_buggy_backup = self._buggy_backup + '.old'
        shutil.copyfile(self._buggy_backup, self._old_buggy_backup)
        shutil.copyfile(self.buggy, self._buggy_backup)

    def transform_buggy(self, defect, suspicious_line):
        if self.make_repairable(self, defect, suspicious_line):
            transform = list(diff(self._buggy_backup, self.buggy))
            if len(transform) != 0:
                return transform

        return None

    def buggy_diff(self):
        return list(diff(self._buggy_backup, self.buggy))

    def repair_diff(self):
        repaired_with_instr_buggy = self.buggy + '.repaired_with_instr'
        shutil.copyfile(self.buggy, repaired_with_instr_buggy)

        shutil.copyfile(self._orig_buggy, self.buggy)

        repair_with_instr_diff = join(self.dir, 'repair_with_instr.diff')
        rc = subprocess.call('diff {} {} > {}'.format(self._with_instr_buggy,
                                                      repaired_with_instr_buggy,
                                                      repair_with_instr_diff),
                             shell=True)
        assert(rc == 0 or rc == 1)
        subprocess.check_call('patch -z .pbak {} {}'.format(self.buggy,
                                                            repair_with_instr_diff),
                              shell=True,
                              stdout=subprocess.DEVNULL,
                              stderr=subprocess.DEVNULL)

        self.tidy(self)

        return list(diff(self._orig_buggy, self.buggy))

    def apply_diff(self, diff_):
        buggy_diff = self.buggy + '.diff'
        with open(buggy_diff, 'w+') as file:
            file.writelines(diff_)

        subprocess.check_call('patch -z .pbak {} {}'.format(self.buggy,
                                                            buggy_diff),
                              shell=True,
                              stdout=subprocess.DEVNULL,
                              stderr=subprocess.DEVNULL)

    def import_compilation_db(self, compilation_db):
        compilation_db = copy.deepcopy(compilation_db)
        for item in compilation_db:
            item['directory'] = join(self.dir, item['directory'])
            item['file'] = join(self.dir, item['file'])
            # this is a temporary hack. It general case, we need (probably) a different workflow:
            wrong_dir = realpath(join(self.dir, '..', 'validation'))
            item['command'] = item['command'].replace(wrong_dir, self.dir)

            item['command'] = item['command'] + ' -I' + os.environ['LLVM3_INCLUDE_PATH']
            # this is a hack to skip output expressions when perform transformation:
            item['command'] = item['command'] + ' -include ' + os.environ['ANGELIX_RUNTIME_H']
            item['command'] = item['command'] + ' -D ANGELIX_INSTRUMENTATION'
        compilation_db_file = join(self.dir, 'compile_commands.json')
        with open(compilation_db_file, 'w') as file:
            json.dump(compilation_db, file, indent=2)

    def configure(self):
        compile_start_time = time.time()
        src = basename(self.dir)
        logger.info('configuring {} source'.format(src))
        if self.configure_cmd is None:
            return
        with cd(self.dir):
            return_code = subprocess.call(self.configure_cmd,
                                          shell=True,
                                          stderr=self.subproc_output,
                                          stdout=self.subproc_output)
        if return_code != 0 and not self.config['mute_warning']:
                logger.warning("configuration of {} returned non-zero code".format(relpath(dir)))
        compile_end_time = time.time()
        compile_elapsed = compile_end_time - compile_start_time
        statistics.data['time']['compilation'] += compile_elapsed


def build_in_env(dir, cmd, subproc_output, config, env=os.environ):
    dirpath = tempfile.mkdtemp()
    messages = join(dirpath, 'messages')

    environment = dict(env)
    environment['ANGELIX_COMPILER_MESSAGES'] = messages

    with cd(dir):
        return_code = subprocess.call(cmd,
                                      env=environment,
                                      shell=True,
                                      stderr=subproc_output,
                                      stdout=subproc_output)
    if return_code != 0 and not config['mute_warning']:
        logger.warning("compilation of {} returned non-zero code".format(relpath(dir)))

    if exists(messages):
        with open(messages) as file:
            lines = file.readlines()
        if not config['mute_warning']:
            for line in lines:
                logger.warning("failed to build {}".format(relpath(line.strip())))

    return return_code


def build_with_cc(dir, cmd, stderr, cc, config):
    env = dict(os.environ)
    env['CC'] = cc
    build_in_env(dir, cmd, stderr, config, env)


class Validation(Project):

    def build(self):
        logger.info('building {} source'.format(basename(self.dir)))
        compile_start_time = time.time()
        rc = build_in_env(self.dir, self.build_cmd,
                          subprocess.DEVNULL if self.config['mute_build_message']
                          else self.subproc_output,
                          self.config)
        compile_end_time = time.time()
        compile_elapsed = compile_end_time - compile_start_time
        statistics.data['time']['compilation'] += compile_elapsed

        return rc


    def export_compilation_db(self):
        logger.info('building json compilation database from {} source'.format(basename(self.dir)))
        compile_start_time = time.time()
        build_in_env(self.dir,
                     'bear ' + self.build_cmd,
                     subprocess.DEVNULL if self.config['mute_build_message']
                     else self.subproc_output,
                     self.config)
        compile_end_time = time.time()
        compile_elapsed = compile_end_time - compile_start_time
        statistics.data['time']['compilation'] += compile_elapsed


        compilation_db_file = join(self.dir, 'compile_commands.json')
        with open(compilation_db_file) as file:
            compilation_db = json.load(file)
        # making paths relative:
        for item in compilation_db:
            item['directory'] = relpath(item['directory'], self.dir)
            item['file'] = relpath(item['file'], self.dir)
        return compilation_db


class Frontend(Project):

    def build(self):
        logger.info('building {} source'.format(basename(self.dir)))
        compile_start_time = time.time()
        build_with_cc(self.dir,
                      self.build_cmd,
                      subprocess.DEVNULL if self.config['mute_build_message']
                      else self.subproc_output,
                      'angelix-compiler --test',
                      self.config)
        compile_end_time = time.time()
        compile_elapsed = compile_end_time - compile_start_time
        statistics.data['time']['compilation'] += compile_elapsed


class Backend(Project):

    def build(self):
        logger.info('building {} source'.format(basename(self.dir)))
        compile_start_time = time.time()
        build_with_cc(self.dir,
                      self.build_cmd,
                      subprocess.DEVNULL if self.config['mute_build_message']
                      else self.subproc_output,
                      'angelix-compiler --klee',
                      self.config)
        compile_end_time = time.time()
        compile_elapsed = compile_end_time - compile_start_time
        statistics.data['time']['compilation'] += compile_elapsed
