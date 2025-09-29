"""
Core functions for managing usernames/passwords in user-specific files
The goal is to avoid exposing login credentials in main pipelining scripts
"""

import getpass
import json
import os
import stat
import sys

# Paths where passwords are stored (Linux only - should detect OS to make
# this Linux compatible)
DEFAULT_PASSWD = os.getenv('HOME') + '/.mngdb/passwd'
#DEFAULT_KEYCHAIN = os.getenv('HOME')+'/.mongopw'
if os.getenv('MONGOPW_FILE') is None:
    DEFAULT_KEYCHAIN = os.getenv('HOME') + '/.mongopw'
else:
    DEFAULT_KEYCHAIN = os.getenv('MONGOPW_FILE')

# TO DO: Would be good to unify the default passwd as part of the keychain
# object

# Function for loading default uname/passwd (see mongo.py::openMongo)


def defaultLogin(pwfile=DEFAULT_PASSWD):
    """
    Load default username/password from a hidden file in user directory.

    Checks for a simple text file with format user:passwd and returns those values as a tuple if found.

    Parameters:
    pwfile: Path to a text file with a single line containing user:passwd. Default is defined at top of module - MAKE SURE TO PROTECT NON-USER PERMISSIONS ON THIS FILE!

    Returns:
    (str,str) Tuple containing user and passwd respectively.
    """
    if pwfile.startswith("~"):
        pwfile = os.getenv('HOME') + pwfile[1:]
    if os.path.exists(pwfile):
        try:
            user, passwd = open(pwfile).read().strip().split(':')
        except ValueError as err:
            print(
                "WARNING: Could not load default login from %s due to ValueError: %s" %
                (pwfile, err), file=sys.stderr)
            return(None, None)
        return (user, passwd)
    else:
        return (None, None)

# Class for loading multiple mongo DB username and passwords, keyed by host and db
# These are expected to be stored in a JSON object containing a single list,
# where each entry is a 4-member dict w/ fields: host, db, user, passwd
# By default, this class loads from ~/.mongopw (MAKE SURE TO REMOVE ALL GROUP/GLOBAL PERMISSIONS FOR THIS FILE!)
# Then any db access functions can check that keychain by default when user/passwd not specified
# TO DO: getPassWd function should have optional username function, for
# when multiple keys to different users on same db are in the keychain
# file


class Keychain:
    """Class for loading multiple mongo DB username and passwords, keyed by host and db"""

    def __init__(self, keyfile=DEFAULT_KEYCHAIN):
        """
        Initialize keychain from a JSON formatted file.

        Load a JSON file containing a single list, where each entry is a 4-member dict w/ fields: host, db, user, passwd. '~' prefix on keyfile will be expanded to user home directory using os.getenv('HOME'). Note that if the specified file is not found, an empty keychain will be created and all get functions will return None.

        Parameters:
        keyfile: Any JSON file matching the structure described above. Default is ~/.mongopw - MAKE SURE TO PROTECT NON-USER PERMISSIONS ON THIS FILE!

        Returns:
        (keychain) A keychain object constructed from the specified file
        """
        if keyfile.startswith("~"):
            keyfile = os.getenv('HOME') + keyfile[1:]
        if os.path.exists(keyfile):
            # Bug fix on 8/11/20 - Use try/catch to handle misformatted JSON
            # file here
            try:
                self.keylist = json.load(open(keyfile, 'r'))
            except json.JSONDecodeError as err:
                print(
                    "WARNING: Could not load keyfile from %s due to JSONDecodeError: %s" %
                    (keyfile, err), file=sys.stderr)
                self.keylist = []
        else:
            self.keylist = []

    def hasKey(self, host, db):
        """
        Check if the keychain has a specified host,db pair.

        Search keychain.keylist for any entry matching host and db

        Parameters:
        host: The mongoDB host
        db: The mongoDB database under the specified host

        Returns:
        (bool) = True if the key is found, False otherwise
        """
        # for key in self.keylist:
        #    if (key['host'] == host) and (key['db'] == db):
        #        return True
        # return False
        return False if self.findKey(host, db) == -1 else True

    def getUser(self, host, db):
        """
        Get a user name from the keychain

        Search keychain.keylist for the first entry matching host and db and return associated user

        Parameters:
        host: The mongoDB host
        db: The mongoDB database under the specified host

        Returns:
        (str) = The requested username, or None if no matching entries found
        """
        # for key in self.keylist:
        #    if (key['host'] == host) and (key['db'] == db):
        #        return key['user']
        # return None
        k = self.findKey(host, db)
        return None if k == -1 else self.keylist[k]['user']

    def getPasswd(self, host, db):
        """
        Get a password from the keychain

        Search keychain.keylist for the first entry matching host and db and return associated passwd

        Parameters:
        host: The mongoDB host
        db: The mongoDB database under the specified host

        Returns:
        (str) = The requested passwd, or None if no matching entries found
        """
        # for key in self.keylist:
        #    if (key['host'] == host) and (key['db'] == db):
        #        return key['passwd']
        # return None
        k = self.findKey(host, db)
        return None if k == -1 else self.keylist[k]['passwd']

    def getKey(self, host, db):
        """
        Get a key from the keychain

        Search keychain.keylist for the first entry matching host and db and return the full dict for that entry in the keychain

        Parameters:
        host: The mongoDB host
        db: The mongoDB database under the specified host

        Returns:
        (dict) = The requested "key" as a dict containing all fields, or None if no matching entries found
        """
        # for key in self.keylist:
        #    if (key['host'] == host) and (key['db'] == db):
        #        # NOTE: the copy function is used here so that changes to key in the calling function do not change what is in the KeyChain object
        #        return key.copy()

        k = self.findKey(host, db)
        return None if k == -1 else self.keylist[k].copy()
        
    def swapPort(self, host):
        """
        small utility to remove port from host if there or add it if not
        Parameters:
        host (str): The mongoDB host
        Returns: the host+port or host-port
        """
        if "27017" in host:
            host = host[:-6]
        else:
            host += ":27017"
        return host 

    def findKey(self, host, db):
        """
        Find the index of specific key matching host,db pair in the keychain.

        Search keychain.keylist for any entry matching host and db, return the index of first entry found.
        
        we first look for an exact match, if not found, we look for a matched host and '*' for db

        Parameters:
        host (str): The mongoDB host
        db (str): The mongoDB database under the specified host

        Returns:
        (int) = Index of first matching key, or -1 if not found.
        """
        # TO DO: Should this function be internal to the class?
                
        for db_name in (db, "*"):     
            for host_name in (host, self.swapPort(host)):     
                for i in range(0, len(self.keylist)):
                    key = self.keylist[i]
                    if (key['host'] == host_name) and (key['db'] == db_name):
                        return i
        
        return -1

    def addKey(
            self,
            host: str,
            db: str,
            user: str,
            passwd: str,
            authSource: str = None,
            authMechanism: str = None):
        """
        Add new key information to the keychain.

        Search for existing key matching host and db to overwrite, otherwise append to end of keychain.keylist.

        Parameters:
        host (str): The mongoDB host
        db (str): The mongoDB database under the specified host
        user (str): The username for mongoDB host/database
        passwd (str): The password associated with username
        authSource (str): The authorization source (optional - if none, leave it out of the key)
        authMechanism (str): The authorization mechanisms (optional - if none, leave it out of the key)
        """
        newKey = {'host': host, 'db': db, 'user': user, 'passwd': passwd}
        if authSource is not None:
            newKey['authSource'] = authSource
        if authMechanism is not None:
            newKey['authMechanism'] = authMechanism
        old = self.findKey(host, db)
        if old == -1:
            self.keylist.append(newKey)
        else:
            self.keylist[old] = newKey

    def save(self, keyfile: str):
        """
        Save contents of this Keychain to a file.

        Writes all keys in Keychain to disk with json.dump, also protects the file so it is only accessible to owner.

        Parameters:
        keyfile (str): Path to file to save, will overwrite existing file.
        """
        json.dump(self.keylist, open(keyfile, 'w'), indent=1)
        # Attempt to change permissions
        os.chmod(keyfile, stat.S_IRUSR | stat.S_IWUSR)


# Attempt to create a default keychain object by default from ~/.mongopw
# If the file is not present in user directory, an empty key chain will be
# created
defkc = Keychain()

# --- Functions to write default/keychain credentials in appropriate paths --- #
# NOTE: These function are meant to be run either in Jupyter notebooks,
# console, or within an interactive command-line script, so status
# messages go straight to stdout

# TO DO: This function should handle exceptions, e.g. if the path to
# pwfile doesn't exist or user doesn't have permissions to overwrite, and
# just print a warning and return False in those cases.


def setDefaultLogin(
        user: str = None,
        passwd: str = None,
        pwfile: str = DEFAULT_PASSWD,
        interactive: bool = False) -> bool:
    """
    Set the default username and password in a secret file in user directory.

    Stores a new default username:password pair in the file specified by DEFAULT_PASSWD, these are either specified as parameters or asked for as prompts in interactive mode. The output file is protected from being read by anyone but owner.

    Parameters:
    user (str): Default username to store. Default is None which must be set with interactive=True.
    passwd (str): Default password to store. Default is None which must be set with interactive=True.
    pwfile (str): Path to file where this will be stored, uses global DEFAULT_PASSWD by default.
    interactive (bool): Prompt for any missing information, defaults to False.

    Returns:
    (bool) = True if new credentials were stored, false otherwise.
    """
    # If interactive mode, ask for missing info
    if interactive:
        if user is None:
            user = input('Default Username:')
        if passwd is None:
            passwd = getpass.getpass(prompt='Default Password:')
        if pwfile is None:
            pwfile = input('Path/File to Store Credentials:')
        # Check if pwfile exists already - if so, prompt user to overwrite or
        # not?
        if os.path.exists(pwfile):
            print('%s already exists.' % pwfile)
            overwrite = None
            while overwrite is None:
                overwrite = input('Overwrite? Y/N:').upper()
                if overwrite[0] == 'Y':
                    print('Overwriting with new login info.')
                elif overwrite[0] == 'N':
                    print('No overwrite, quitting without setting new login defaults.')
                    return False
                else:
                    overwrite = None
    else:
        if user is None:
            print(
                "No user specified in non-interactive mode, no default login will be set.")
            return False
        if passwd is None:
            print(
                "No passwd specified in non-interactive mode, no default login will be set.")
            return False
        if pwfile is None:
            print(
                "No pwfile specified in non-interactive mode, no default login will be set.")
            return False
        if os.path.exists(pwfile):
            print('%s already exists - will overwrite.' % pwfile)
    # At this point, all required params were either set through function call
    # or prompted to user
    open(pwfile, 'w').write('%s:%s\n' % (user, passwd))
    # Attempt to change permissions
    os.chmod(pwfile, stat.S_IRUSR | stat.S_IWUSR)
    print('Wrote new default login credentials to private file: %s' % pwfile)
    return True


def addKeychainEntry(
        host: str = None,
        db: str = None,
        user: str = None,
        passwd: str = None,
        authSource: str = None,
        authMechanism: str = None,
        keyfile: str = DEFAULT_KEYCHAIN,
        interactive: bool = False) -> bool:
    """
    Add a new login key to existing keychain on disk.

    Save host,db,user,passwd entry to the keychain represented by keyfile and write back to disk. When interactive=True, prompt user for any missing parameters. If there's already a keychain entry for host,db, overwrite it instead of adding a new one. The keychain object is read from disk within this function, the new key is added, and the whole keychain is written back to disk. The new key is not accessible to other parts of the code until the keychain is reloaded from disk.

    Parameters:
    host (str): The mongoDB host
    db (str): The mongoDB database under the specified host
    user (str): The username for mongoDB host/database
    passwd (str): The password associated with username
    authSource (str): The authorization source associated with host/database (optional)
    authMechanism (str): The authorization mechanism associated with host/database (optional)
    keyfile (str): The keychain to load from disk, defaults to global DEFAULT_KEYCHAIN
    interactive (bool): Should the function prompt user for missing key info, defaults to False

    Returns:
    (bool) = True if the new key was added successfully, False otherwise
    """
    # If interactive, prompt for any missing params
    if interactive:
        if host is None:
            host = input('Host: ')
        if db is None:
            db = input('Database Name: ')
        if user is None:
            user = input('%s/%s Username: ' % (host, db))
        if passwd is None:
            passwd = getpass.getpass(prompt='%s/%s Password: ' % (host, db))
        if authSource is None:
            authSource = input(
                '%s/%s Authorization Source (Leave Blank to Skip):' %
                (host, db))
            # Change blank to None here
            if authSource == "":
                authSource = None
        if authMechanism is None:
            authMechanism = input(
                '%s/%s Authorization Mechanism (Leave Blank to Skip):' %
                (host, db))
            # Change blank to None here
            if authMechanism == "":
                authMechanism = None
        if keyfile is None:
            keyfile = input('Path/File to Store Keychain: ')
    # If not interactive, check for missing params
    else:
        if host is None:
            print("No host specified in non-interactive mode, cannot add to keychain.")
            return False
        if db is None:
            print("No db specified in non-interactive mode, cannot add to keychain.")
            return False
        if user is None:
            print("No user specified in non-interactive mode, cannot add to keychain.")
            return False
        if passwd is None:
            print("No passwd specified in non-interactive mode, cannot add to keychain.")
            return False
        if keyfile is None:
            print("No keyfile specified in non-interactive mode, cannot add to keychain.")
            return False
    # Load the keyfile into Keychain object
    mykc = Keychain(keyfile)
    # Check if this will overwrite an existing key
    if mykc.hasKey(host, db):
        if interactive:
            # In interactive mode, prompt the user if this is OK
            print(
                '%s already contains login key for %s/%s' %
                (keyfile, host, db))
            overwrite = None
            while overwrite is None:
                overwrite = input('Overwrite? Y/N:').upper()
                if overwrite[0] == 'Y':
                    print('Overwriting with new login info.')
                elif overwrite[0] == 'N':
                    print('No overwrite, quitting without setting new login key.')
                    return False
                else:
                    overwrite = None
        else:
            # Non-interactive mode, just print a message that a key is being
            # overwritten
            print(
                'Overwriting existing login key for %s/%s in %s' %
                (host, db, keyfile))
    # Add the new key
    mykc.addKey(host, db, user, passwd, authSource, authMechanism)
    # Write back to disk
    mykc.save(keyfile)
    print('Wrote updated keychain to private file: %s' % keyfile)
