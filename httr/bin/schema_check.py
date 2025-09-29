
import datetime
import os
import sys

TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-2])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB

from schema_checks import dbs_schema_checker, errors

# creating a dbs_schema_checker and telling it to check everything
# alternativaly, we could just call the dbs_schema_checker on 1 db only or
# on 1 type of collection accross all the dbs

# Change the target database here, by default uses the TEST_DB environment variable
test_db = os.getenv("TEST_DB")

# examples of usage

# creating main object:
schema_checker = dbs_schema_checker()

# 1 - checking data integrity for all collections in the DB specified by test_db

t = datetime.datetime.now()
res = schema_checker.check_data_integrity(db_name=test_db)
for item in res:
    print(item)
print(
    f"validating check_data_integrity for {test_db} took {datetime.datetime.now() - t}")

# 2 check sample_ids only

res = schema_checker.check_sample_ids(db_name=test_db)
for item in res:
    print(item)

# 3 - checking all collections in the DB specified by test_db
t = datetime.datetime.now()
res = schema_checker.check_db(test_db, type_check=True)
for item in res:
    print(item)
print(
    f"validating all collections in {test_db} with type_check on took {datetime.datetime.now() - t}")


# clear error logs
errors.clear()

# All dbs operations

# checking all dbs all collections
# schema_checker.check()

#  checking 'httr_study' accross all dbs
# schema_checker.check_col('httr_study', type_check = True)
# print(f"validating httr_study accross all dbs with type_check on took {datetime.datetime.now() - t}")

# clear error logs
# errors.clear()
