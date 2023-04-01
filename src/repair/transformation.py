import os
import sys
from os.path import join, basename, relpath
import tempfile
import subprocess
from utils import cd
import logging
import shutil


logger = logging.getLogger(__name__)


class TransformationError(Exception):
    pass


class BuggyTransformer:

    def __init__(self, config):
        self.config = config
        if self.config['verbose']:
            self.subproc_output = sys.stderr
        else:
            self.subproc_output = subprocess.DEVNULL

    def __call__(self, project, defect, suspicious_line):
        src = basename(project.dir)
        logger.info('making {} source repairable'.format(src))
        environment = dict(os.environ)
        if defect == 'missing-returns':
            environment['ANGELIX_MISSING_RETURNS_DEFECT_CLASS'] = 'YES'
        if defect == 'if-to-elseifs':
            environment['ANGELIX_IF_TO_ELSEIFS_DEFECT_CLASS'] = 'YES'
        else:
            return False
        environment['ANGELIX_SUSPICIOUS_LINE'] = str(suspicious_line)
        with cd(project.dir):
            return_code = subprocess.call(['make-repairable', project.buggy],
                                          stderr=self.subproc_output,
                                          stdout=self.subproc_output,
                                          env=environment)
        if return_code != 0:
            if self.config['ignore_trans_errors']:
                logger.warning("transformation of {} failed".format(relpath(project.dir)))
            else:
                logger.error("transformation of {} failed".format(relpath(project.dir)))
                raise TransformationError()

        return True


class RepairableTransformer:

    def __init__(self, config):
        self.config = config
        if self.config['verbose']:
            self.subproc_output = sys.stderr
        else:
            self.subproc_output = subprocess.DEVNULL

    def __call__(self, project):
        src = basename(project.dir)
        logger.info('instrumenting repairable of {} source'.format(src))
        environment = dict(os.environ)
        if 'if-conditions' in self.config['defect']:
            environment['ANGELIX_IF_CONDITIONS_DEFECT_CLASS'] = 'YES'
        if 'assignments' in self.config['defect']:
            environment['ANGELIX_ASSIGNMENTS_DEFECT_CLASS'] = 'YES'
        if 'loop-conditions' in self.config['defect']:
            environment['ANGELIX_LOOP_CONDITIONS_DEFECT_CLASS'] = 'YES'
        if 'deletions' in self.config['defect']:
            environment['ANGELIX_DELETIONS_DEFECT_CLASS'] = 'YES'
        if 'guards' in self.config['defect']:
            environment['ANGELIX_GUARDS_DEFECT_CLASS'] = 'YES'
        if 'return-values' in self.config['defect']:
            environment['ANGELIX_RETURN_VALUES_DEFECT_CLASS'] = 'YES'
        if self.config['ignore_trivial']:
            environment['ANGELIX_IGNORE_TRIVIAL'] = 'YES'
        if self.config['semfix']:
            environment['ANGELIX_SEMFIX_MODE'] = 'YES'
        if self.config['use_semfix_syn']:
            environment['ANGELIX_USE_SEMFIX_SYN'] = 'YES'
        with cd(project.dir):
            return_code = subprocess.call(['instrument-repairable', project.buggy],
                                          stderr=self.subproc_output,
                                          stdout=self.subproc_output,
                                          env=environment)
        if return_code != 0:
            
            if self.config['ignore_trans_errors']:
                logger.warning("transformation of {} failed".format(relpath(project.dir)))
            else:
                logger.error("transformation of {} failed".format(relpath(project.dir)))    
                raise TransformationError()


class SuspiciousTransformer:

    def __init__(self, config, extracted):
        self.config = config
        self.extracted = extracted
        if self.config['verbose']:
            self.subproc_output = sys.stderr
        else:
            self.subproc_output = subprocess.DEVNULL

    def __call__(self, project, expressions):
        src = basename(project.dir)
        logger.info('instrumenting suspicious of {} source'.format(src))
        environment = dict(os.environ)
        dirpath = tempfile.mkdtemp()
        suspicious_file = join(dirpath, 'suspicious')
        with open(suspicious_file, 'w') as file:
            for e in expressions:
                file.write('{} {} {} {}\n'.format(*e))

        if self.config['semfix']:
            environment['ANGELIX_SEMFIX_MODE'] = 'YES'

        if self.config['synthesis_global_vars']:
            environment['ANGELIX_GLOBAL_VARIABLES'] = 'YES'

        if self.config['synthesis_func_params']:
            environment['ANGELIX_FUNCTION_PARAMETERS'] = 'YES'

        if self.config['synthesis_used_vars']:
            environment['ANGELIX_USED_VARIABLES'] = 'YES'

        if self.config['synthesis_ptr_vars']:
            environment['ANGELIX_POINTER_VARIABLES'] = 'YES'

        if self.config['init_uninit_vars']:
            environment['ANGELIX_INIT_UNINIT_VARS'] = 'YES'

        environment['ANGELIX_EXTRACTED'] = self.extracted
        environment['ANGELIX_SUSPICIOUS'] = suspicious_file

        with cd(project.dir):
            return_code = subprocess.call(['instrument-suspicious', project.buggy],
                                          stderr=self.subproc_output,
                                          stdout=self.subproc_output,
                                          env=environment)
        if return_code != 0:
            if self.config['ignore_trans_errors']:
                logger.warning("transformation of {} failed".format(relpath(project.dir)))
            else:
                logger.error("transformation of {} failed".format(relpath(project.dir)))
                raise TransformationError()

        shutil.rmtree(dirpath)


class FixInjector:

    def __init__(self, config):
        self.config = config
        if self.config['verbose']:
            self.subproc_output = sys.stderr
        else:
            self.subproc_output = subprocess.DEVNULL

    def __call__(self, project, patch):
        src = basename(project.dir)
        logger.info('applying patch to {} source'.format(src))

        environment = dict(os.environ)
        dirpath = tempfile.mkdtemp()
        patch_file = join(dirpath, 'patch')
        with open(patch_file, 'w') as file:
            for e, p in patch.items():
                file.write('{} {} {} {}\n'.format(*e))
                file.write(p + "\n")

        if self.config['semfix']:
            environment['ANGELIX_SEMFIX_MODE'] = 'YES'

        environment['ANGELIX_PATCH'] = patch_file

        with cd(project.dir):
            return_code = subprocess.call(['apply-patch', project.buggy],
                                          stderr=self.subproc_output,
                                          stdout=self.subproc_output,
                                          env=environment)
        if return_code != 0:
            if self.config['ignore_trans_errors']:
                logger.error("transformation of {} failed".format(relpath(project.dir)))
            else:
                logger.error("transformation of {} failed".format(relpath(project.dir)))
                raise TransformationError()

        shutil.rmtree(dirpath)

        pass

class PrintfTransformer:

    def __init__(self, config):
        self.config = config
        if self.config['verbose']:
            self.subproc_output = sys.stderr
        else:
            self.subproc_output = subprocess.DEVNULL

    def __call__(self, project, source_file):
        src = basename(project.dir)
        logger.info('instrumenting printfs of {} source'.format(src))

        with cd(project.dir):
            return_code = subprocess.call(['instrument-printf', source_file],
                                          stderr=self.subproc_output,
                                          stdout=self.subproc_output)
            with open(source_file, 'r+') as f:
                content = f.read()
                f.seek(0, 0)
                f.write('#ifndef ANGELIX_OUTPUT\n#define ANGELIX_OUTPUT(type, expr, id) expr\n#endif\n' + content)

        if return_code != 0:
            if self.config['ignore_trans_errors']:
                logger.error("transformation of {} failed".format(relpath(project.dir)))
            else:
                logger.error("transformation of {} failed".format(relpath(project.dir)))
                raise TransformationError()

        pass


class RepairedTransformer:

    def __init__(self, config):
        self.config = config
        if self.config['verbose']:
            self.subproc_output = sys.stderr
        else:
            self.subproc_output = subprocess.DEVNULL

    def __call__(self, project):
        src = basename(project.dir)
        logger.info('tidying {} source'.format(src))
        environment = dict(os.environ)
        with cd(project.dir):
            return_code = subprocess.call(['tidy', project.buggy],
                                          stderr=self.subproc_output,
                                          stdout=self.subproc_output,
                                          env=environment)
        if return_code != 0:
            if self.config['ignore_trans_errors']:
                logger.warning("transformation of {} failed".format(relpath(project.dir)))
            else:
                logger.error("transformation of {} failed".format(relpath(project.dir)))
                raise TransformationError()
