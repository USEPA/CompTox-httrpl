"""
Core functions for httr-pipeline (httrpl)
Created on 9/3/19 by LJE

History:
9/3/19 - LJE - Moved some general utility functions from gexp/biospyder2.py into this module
"""

import datetime
import shlex
import sys


# Class for combining log output stream and basic logging parameters
# The intention is that an object of this type is created when a script or
# thread starts up, and is then passed to all functions that might need to
# log output
class PipelineLogger:
    """Class for combining log output stream and basic logging parameters"""

    def __init__(
            self,
            out=sys.stderr,
            dbg: bool = True,
            strict: bool = True,
            autoflush: bool = True):
        """
        Create an object for logging output with fixed settings

        Create a PipelineLogger object. The intention is that one object of this type should be created at the beginning of a script or thread with specific settings, and is then passed to all functions that might need to log output.

        Parameters:
        out (output stream) = Where log messages will be written to. Can be a file in write-mode. Defaults to STDERR.
        dbg (bool) = Should debug-level messages be written to this log? Defaults to True.
        strict (bool) = Should warning messages be treated as errors?
        autoflush (bool) = Should every message also be followed by a flush() call on the output stream? (Useful for long-running scripts)
        """
        self.out = out
        self.dbg = dbg
        self.strict = strict
        self.autoflush = autoflush

    def write(
            self,
            msg: str,
            warn: bool = False,
            dbg: bool = False,
            timestamp: bool = False):
        """
        Write a message to this log

        Write a message to this log, with optional specifications and dependent on object parameters. Always appends a new line.

        Parameters:
        msg (str) = The string to write to the log (NOTE: a new line is ALWAYS appended automatically, like print function)
        warn (bool) = Is this a warning message? (if set to True, will append WARNING prefix and quit in strict mode)
        dbg (bool) = Is this a debug message? (if set to True, will only write if self.dbg is also True)
        timestamp (bool) = Should this message be timestamped? (if set to True, append a timestampt to front of message)
        """
        # Bail out if this is a debug message but logger is not in dbg mode
        if dbg and not self.dbg:
            return
        # Append optional timestamp
        if timestamp:
            msg = "[%s] %s" % (datetime.datetime.now().strftime(
                '%Y-%m-%d %H:%M:%S'), msg)
        # Append optional WARNING prefix
        if warn:
            msg = "WARNING: " + msg
        # Write message to log
        self.out.write(msg + "\n")
        # Optional auto-flush of output stream
        if self.autoflush:
            self.out.flush()
        # Quit on warning in strict mode:
        if warn and self.strict:
            sys.exit("ERROR: Quit on warnings in strict mode.\n")

    def warning(self, msg: str, dbg: bool = False):
        """
        Write a warning message to this log

        Convenience function - passes relevant parameters to write with warn=True

        Parameters:
        msg (str) = The string to write to the log (NOTE: a new line is ALWAYS appended automatically, like print function)
        dbg (bool) = Is this a debug message? (if set to True, will only write if self.dbg is also True)
        """
        self.write(msg=msg, warn=True, dbg=dbg, timestamp=False)

    def error(self, msg: str):
        """
        Write a message to this log and then quit

        Appends an ERROR prefix to message, writes to log, and then calls sys.exit with same message.

        Parameters:
        msg (str) = The string to write to the log (NOTE: a new line is ALWAYS appended automatically, like print function)
        """
        self.write(msg="ERROR: " + msg, warn=False, dbg=False, timestamp=False)
        sys.exit("ERROR: " + msg)


# Create a default logger - uses all default params in constructor function
deflog = PipelineLogger()

# Convenience function for cleaning up file paths that contain escape characters
# Optionally pushes warnings to log stream if the path string changes


def cleanPath(
        path: str,
        warn: bool = True,
        log: PipelineLogger = deflog) -> str:
    """
    Attempt to clean up issues with escape characters in paths

    Some functions, like subprocess.X methods, will accept escape characters in paths, but other functions, such as open(), will not. This function attempts to use shlex module to clean up escape characters in the path. Reports a debug warning message if the path changes (can also be downgraded from a warning to avoid triggering strict when not needed).

    Parameters:
    path (str) = The path to check and optionally clean up
    warn (bool) = Should the message be passed to log as a warning or normal message? True by default.
    log (PipelineLogger) = Logging object for warnings

    Return Type:
    (str) = The path with any problematic escape chars removed
    """
    # Pass path through shlex tokenizer to clean up backslashes:
    newPath = ' '.join(shlex.split(path, posix=True))
    # If path changed, report as appropriate
    if newPath != path:
        msg = "path contains backslashes - this is not necessary, attempting to convert to POSIX-compatible string with shlex.\n         \
            Original path  = %s\n         \
            Corrected path = %s" % (
            path, newPath)
        if warn:
            log.warning(msg, dbg=True)
        else:
            log.write(msg, dbg=True)
        path = newPath
    return path

