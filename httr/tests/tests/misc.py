'''
This "Expected" class is a generic class that basically will compare values from a collection to
pre-loaded values from a file and report accordingly
It has some flexibility is that it allows for the callers (most test classes) to configure their Expected instance according
to the location of its relevant data, the name of the relevant collections, the sample_ids to be focusing on ...
The class also has two modes of operation based on its 'calibrate' value, which allows to compare collection data with saved data on HD
when calibrate if Falsfe, or allows to write out the collection data to HD when calibrate is True.

'''


import json
import os

from deepdiff import DeepDiff


class Expected:
    def __init__(self, DB):
        self.DB = DB
        self.sample_data = {"test_missing_fastq": ["TC00283171_L10_2", "TC00283171_J02_2"],
                            "test_main": ["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_warnings": ["TC00283209_E07_2", "TC00283171_H13_2"],
                            "test_norerun": ["TC00283171_C09_2", "TC00283209_K19_2"],
                            "test_qcBatch": ["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_countstats": ["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_countgini": ["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_countqcflag": ["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_update_qcFlag": ["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_qcBatch_filter": ["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_fill_dose_welltrt": ["TC0000490_G15","TC0000490_N14"],
                            "test_insertmany_degs":[1, 2],
                            "test_fastqGC_process":["TC00283151_D21_2", "TC00283151_C16_2"],
                            "test_update_gc_frac":["TC00283151_D21_2", "TC00283151_C16_2"],
                            }

        self.expected = {"data/expected_counts.json": {"expected_value": "",
                                                       "coll_name": "httr_counts",
                                                       "projection": {"raw_id": 0,
                                                                      "run_time": 0,
                                                                      "_id": 0,
                                                                      "host_name": 0},
                                                       "part_of_batch": True,
                                                       "pk":"sample_id"},
                         "data/expected_raw.json": {"expected_value": "",
                                                    "coll_name": "httr_raw",
                                                    "projection": {"_id": 0,
                                                                   "mtime": 0},
                                                    "part_of_batch": True,
                                                    "pk":"sample_id"},
                         "data/expected_counts_qc.json": {"expected_value": "",
                                                          "coll_name": "httr_counts_qc",
                                                          "projection": {"_id": 0,
                                                                         "count_id": 0},
                                                          "part_of_batch": False,
                                                          "pk":"sample_id"},
                         "data/expected_countstats.json": {"expected_value": "",
                                                           "coll_name": "httr_counts_stats",
                                                           "projection": {"_id": 0},
                                                           "part_of_batch": False,
                                                           "pk":"sample_id"},
                         "data/expected_countgini.json":       {"expected_value": "",
                                                                "coll_name": "httr_counts_stats",
                                                                "projection": {"_id": 0},
                                                                "part_of_batch": False,
                                                                "pk":"sample_id"},
                         "data/expected_countqcflag.json":     {"expected_value": "",
                                                                "coll_name": "httr_counts_stats",
                                                                "projection": {"_id": 0},
                                                                "part_of_batch": False,
                                                                "pk":"sample_id"},
                         "data/expected_update_qcFlag.json":   {"expected_value": "",
                                                                "coll_name": "httr_counts_qc",
                                                                "projection": {"_id": 0,
                                                                               "count_id": 0,
                                                                               "update_history": 0},
                                                                "part_of_batch": False,
                                                                "pk":"sample_id"},
                         "data/expected_counts_qc_filter.json": {"expected_value": "",
                                                                 "coll_name": "httr_counts_qc",
                                                                 "projection": {"_id": 0,
                                                                                "count_id": 0},
                                                                "part_of_batch": False,
                                                                "pk":"sample_id"},
                         "data/expected_fill_dose_welltrt.json": {"expected_value": "",
                                                                 "coll_name": "httr_dose_httr_well_trt",
                                                                 "projection": {"_id": 0},
                                                                 "part_of_batch": False,
                                                                 "pk":"sample_id"},                                                                                            
                         "data/expected_insertmany_degs.json": {"expected_value": "",
                                                                 "coll_name": "httr_deg",
                                                                 "projection": {"_id": 0,
                                                                                "update_notes": 0,
                                                                                "run_time": 0},
                                                                 "part_of_batch": False,
                                                                 "pk":"X"},  
                         "data/expected_fastqGC.json":         {"expected_value": "",
                                                                 "coll_name": "httr_fastqGC",
                                                                 "projection": {"_id": 0,
                                                                                "update_notes": 0,
                                                                                "run_time": 0},
                                                                 "part_of_batch": False,
                                                                 "pk":"sample_id"},                                                                 
                         "data/expected_test_update_gc_frac.json":    {"expected_value": "",
                                                                 "coll_name": "httr_raw",
                                                                 "projection": {"_id": 0,
                                                                                "mtime": 0},
                                                                 "part_of_batch": False,
                                                                 "pk":"sample_id"}}

    def deep_diff(self, expected, result, *, what=None, **deep_diff_kwargs):
        """Compare expected and actual results
        wrapper around the deepDiff library that allows to compare python dictionaries
        PARAMETERS:
        expected:str  the expected string
        result:str   the result to compare expected with
        what: str  keyword to tell remind us what was compared (if provided)
        deep_diff_kwargs:dict   other parameters we wish to pass to DeepDiff such as exclude_paths

        RETURN:
        NONE, but call assert

        """

        assert expected
        assert result

        diff = DeepDiff(expected, result, **deep_diff_kwargs)
        if diff:
            if what is not None:
                print(f"Failed on: {what}", flush=True)
            print(diff.pretty(), flush=True)
            assert expected == result

    def load_expected(self, calibrate, test_name):
        '''
        load_expected calls load_expected_coll for each collection related to the provide test_namee
        that is marked as 'part of batch', mainly httr_raw and httr_counts

        PARAMETERS:
        calibrate: boolean  should be False
        test_name:str  the test name we are gathering information for
        RETURN:
        NONE, simply the 'expected' key associated with the collection is filled with the data string read by
        load_expected_coll
        '''

        if not calibrate:
            for key in self.expected:
                if self.expected[key]["part_of_batch"]:
                    self.load_expected_coll(calibrate, test_name, key)

    def load_expected_coll(self, calibrate, test_name, key):
        '''
        load_expected_coll reads the file associated with the test name and loads its content into the 'expected' key
        associated with the collection key in the expected dictionary

        PARAMETERS:
        calibrate: boolean  should run only when we are not calibrating
        test_name:str  the test name we are gathering information for
        RETURN:
        NONE, simply the 'expected' key associated with the collection is filled with the data string read
        '''

        if not calibrate:
            dot = key.find('.')
            with open(os.path.join(os.path.dirname(__file__), '../' + key[:dot] + test_name + key[dot:]), 'r') as f:
                self.expected[key]["expected_value"] = json.load(f)

    def write_expected_coll(self, test_name, start, key):
        '''
        write_expected_coll writes the data into the file identified by the test_name

        PARAMETERS:
        test_name:str  the test name to identify the dictionary key in expected and figure out the name of the file to write out to
        start:int the first sample_id whose data needs to be written (we sometimes skip the first one)
        key:str   the specific key in the key:value dictionary to be written out along with the value data from the document in the collection
        associated with the test

        RETURN:
        NONE

        '''
        dot = key.find('.')
        f = open(
            os.path.join(
                os.path.dirname(__file__),
                '../' + key[: dot] + "_" + test_name + key[dot:]),
            'w')
        f.write('{')

        tmp_dict = {}
        pk  = self.expected[key]["pk"]
        for elem in self.sample_data[test_name][start:]:
            res = self.DB[self.expected[key]["coll_name"]].find_one(
                {pk: elem}, self.expected[key]["projection"])
            print(f"res is {res} pk is {pk} elem is {elem} key is {key}")
            if res:
                s = json.dumps(res, indent=4, sort_keys=True)
                tmp_dict[f'\"{elem}\"'] = s
        f.write(
            ",".join(
                f"{key} : {value}" for key,
                value in tmp_dict.items()))
        f.write('}')
        f.close()

    def write_expected(self, test_name, start=0):
        '''
        write_expected writes out when CALIBRATE is set to Y for the test_name provided for the sample_ids found starting with 'start'
        for the collections for which part_of_batch is True

        PARAMETERS:
        test_name:str  the name of the test for which we want to write data out
        start:int the first item in the sample_data[test_name] dictionary for which to write data

        RETURN:
        None, only writes out to HD the dictionary with the document found in collection

        '''
        for key in self.expected:
            if self.expected[key]["part_of_batch"]:
                self.write_expected_coll(test_name, start, key)

    def check_expected(self, test_name, start=0, normal_processing=True):
        '''
        check_expected will find the data related to the test_name provided and get it checked against expected
        PARAMETER:
        test_name:str  the test name use to identify the data related and the json expected file
        start: the first item in the sample_data dictionary for that test_name to look up (ignoring sample_ids prior to start)
        normal_processing: if True then compare with the expected string, if False compare with None (We do not expect any data to be returned)

        RETURN:
        None, the most inner function (check_expected_doc) will do the asserts
        '''
        for key in self.expected:
            if self.expected[key]["part_of_batch"]:
                self.check_expected_coll(
                    test_name, start, key, normal_processing)

    def no_probes(self,data):
        filtered={}
        for k,v in data.items():
            if k!="probe_cnts":
                filtered[k]=v
        return filtered
                
    def check_expected_doc(
            self,
            expected_data,
            doc_id,
            normal_processing=True):
        '''
        check_expected_doc checks a given document against its expected value

        PARAMETERS:
        expected_data:str  the key in the expected dictionary to find the info related to this operation
        doc_id: the sample_id value we are processing
        normal_processing:boolean   if True then will compare against the related expected value, if False will compare against None

        '''
            
        pk  = self.expected[expected_data]["pk"]
        res = self.DB[self.expected[expected_data]["coll_name"]].find_one(
            {pk: doc_id}, self.expected[expected_data]["projection"])
        if normal_processing:
            print(
                f"check_expected_coll for {doc_id} in {self.expected[expected_data]['coll_name']}",
                flush=True)
            #self.DB[self.expected[expected_data]["coll_name"]].print_collection()
            assert res
            if type(doc_id) != str:   doc_id=str(doc_id) #converted to str cause json.loads wants str keys only
            print(f"self.expected[expected_data]['expected_value'] is {self.no_probes(self.expected[expected_data]['expected_value'][doc_id])}")
            self.deep_diff(
                res,
                self.expected[expected_data]["expected_value"][doc_id],
                exclude_paths=[
                    f"root['path']",
                    "root['aln_cmd']"])
        else:
            assert res is None

    def check_expected_coll(
            self,
            test_name,
            start,
            key,
            normal_processing=True):
        '''
        for each sample_id for the considered test, will read that sample_id data from related collection into the
        target file associated with that test and compare it with the expected string

        PARAMETERS:
        test_name: str the test_name to identify the samples to look for
        start:int the index of the first sample to start looking for
        key:str  the json file used as key to get to the data related to our checking
        normal_processing: boolean   when true will compare with the data found in expected, when false with compare with None

        Return:
        None, the most inner function check_expected_doc performs the asserts


        '''
        for elem in self.sample_data[test_name][start:]:
            self.check_expected_doc(
                expected_data=key, doc_id=elem,
                normal_processing=normal_processing)

    def handle_expected_coll(self, test_name, start, exp, calibrate):
        '''
        handle_expected_coll is a distpatch function for a given sample_id start index and a specific expected file
        for example: data/expected_counts_qc.json

        PARAMETERS:
        test_name:str  the test_name to id the sample_ids
        start:int the index where to start in the sample_ids list in the expected dictionary
        exp: the expected file to use to key in the expected structure to find the parameters related to the operation (i.e.
            expected_value, coll_name, projection, part_of_batch)
        calibrate:boolean  used to direct traffic between checking against expected value or writing a new expected value
        '''

        if calibrate:
            self.write_expected_coll(test_name, start, exp)
        else:
            self.check_expected_coll(test_name, start, exp)

    def handle_expected(self, test_name, calibrate, normal_processing=True):
        '''
        handle_expected is a distpatch function for a given sample_id list

        PARAMETERS:
        test_name:str  the test_name to id the sample_ids
        calibrate:boolean  used to direct traffic between checking against expected value or writing a new expected value
        normal_processing:boolean  if True then will compare against the related expected value, if False will compare against None

        RETURNS:
        None, but check_expected_doc witll run an assert - write_expected won't
        '''

        print(f"normal processing is {normal_processing}", flush=True)
        if calibrate:
            if normal_processing:
                self.write_expected(test_name)
        else:
            self.check_expected(test_name, normal_processing=normal_processing)


def configLoader(al):
    '''
    configLoader will load the align_and_count object config file and retur it after filling out potentially missing info

    PARAMETERS:
    al:align_and_count class instance to update its db_host/db_name if abscent
    RETURN:
    Nothing

    '''

    if 'db_host' not in al.config or al.config['db_host'] is None:
        al.config['db_host'] = os.getenv("TEST_HOST")
    if 'db_name' not in al.config or al.config['db_name'] is None:
        al.config['db_name'] = os.getenv("TEST_DB")


def check_and_build_httr_probe(DB):
    '''
    check_and_build_httr_probe detects if the httr_probe collection is present and that it has the expected number of documents
    and if not it recreates it from a json file

    PARAMETERS:
    DB: mongo DB object

    Returns:
    None, will created and populated the httr_probe collection if needed

    '''
    HTTR_PROBE_DOC_NB = 21111
    print(
        f"DB['httr_probe'].count is {DB['httr_probe'].count({},{})}",
        flush=True)
    if 'httr_probe' not in DB.list_collection_names(
    ) or DB['httr_probe'].count({}, {}) != HTTR_PROBE_DOC_NB:
        if 'httr_probe' in DB.list_collection_names():
            print("dropping httr_probe", flush=True)
            DB['httr_probe'].drop()
        content = json.load(
            open(
                'data/httrpl_automationTestData/httr_probe_data/httr_mcf7_pilot_probe.json',
                'r'))
        DB['httr_probe'].insert_many(content)
