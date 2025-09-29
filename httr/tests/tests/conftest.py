"""
Define common fixtures here.
Brian Okken's 'Python Testing with Pytest' used as reference
"""

import os

import pytest


@pytest.fixture()
def get_env_var():
    """gets the environment variable.
    If variable DNE, return default if provided else raise an Exception.
    This uses "factory as fixture" pattern.

    Parameter:
    None
    Return
    env_name value: str
    """

    def _get_env_var(env_name, default=None):
        env_var = os.environ.get(env_name, default)
        if env_var is None:
            raise Exception(
                "The environment variable %s is not defined" % env_name
            )
        return env_var.rstrip('/')

    return _get_env_var


@pytest.fixture()
def calibrate(get_env_var):
    """
    calibrate ficture simply returns the value in the CALIBRATE env. var.
    PARAMETER:
    get_env_var: function
    Returns:
    get_env_var variable: boolean True if CALIBRATE is true

    """
    return (get_env_var("CALIBRATE", "No") in ('1', 'Yes', 'yes', 'Y', 'y'))
