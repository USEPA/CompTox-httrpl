
import json
import os
from datetime import date, datetime
import inspect
import copy


class json_DB:

    """
        represents the json db taht contains collections represented by files   
    """

    def __init__(self, output_dir):
        """
        the db is created with an output_dir folder taht will be created if abscent
        """
        
        self.output_dir = output_dir
        self.collections = {}
        self.name = ""
        
        if not os.path.isdir(self.output_dir):
            os.mkdir(self.output_dir)
        
    def list_collection_names(self):  
        """
        Parameter:
        None
        Return:
        a list of collection names found at output_dir
        """
        
        if os.path.isdir(self.output_dir):
            return([f for f in os.listdir(self.output_dir)])
        else:
            os.mkdir(self.output_dir)
            return []
        
    def __getitem__(self, items):
        """
        Parameter:
        the collection name to look for
        Return:
        the collection object corresponding to the provided name
        creates the collection object if not found
        """

        if items in self.collections:
            return self.collections[items].data
        else:
            collection = json_DB_collection(self.output_dir, items)
            return collection
            
    def __getattr__(self, items):
        """
        Parameter:
        the collection name to look for. This is called when an attribute is passed using the . notation
        and the attribute doesn't exist
        We use that to allow for calls like DB.<collection> to be treated just like
        DB['collection']
        The catch is that if a wrong attribute is used on the DB, this will assume this is a collection that 
        either exists or needs to be created
        Return:
        the collection object corresponding to the provided name
        creates the collection object if not found
        """
        if items in self.collections:
            return self.collections[items].data
        else:
            collection = json_DB_collection(self.output_dir, items)
            return collection
        
class json_DB_collection:
    def __init__(self, output_dir, name):
        """
        Parameter:
        the output_dir folder to interact with
        the name of the collection to be created
        Return:
        Nothing
        """
        
        self.name = name
        self.filename = os.path.join(output_dir, name)
        self.load_data()
        
    def drop(self):
        """
        finds the collection file on disk and removes it
        Parameter:
        Nothing
        Return:
        Nothing
        """
        
        if os.path.isfile(self.filename):
            os.remove(self.filename)
            
    def print_collection(self):
        """
        Tool to print the collection
        Parameter:
        Nothing
        Return:
        Nothing
        """
 
        for row in self.data:
            r={}
            for k,v in row.items():
                if k!="probe_cnts":
                    r[k]=v
            print(f"r is {r}")
        
    def save(self):
        """
        commits the collection to disk
        Parameter:
        Nothing
        Return:
        Nothing
        """
 
        with open(self.filename, "w") as f:
            for item in self.data:
                for field in item:
                    if isinstance(item[field], (datetime, date)):
                        item[field] = item[field].isoformat()
            json.dump(self.data, f)
  
        
    def find(self, query={}, filter={}, row_nb=[]):
        """
        finds rows in the collection data corresponding to query and limited to fields passed in filter
        Parameter:
        query: dict that contains the query
        filter: dict that lays out the fields to include/exclude and also handles $in/$exists functions
        row_nb: list of row numbers corresponding the found items. Handled by the caller
        Return:
        The list of rows as a list of dicts
        """
        
        rows = []
        data = copy.deepcopy(self.data)
        if len(query)==0:
            rows = data
        else:
            for index, row in enumerate(data):
                for key, val in query.items():
                    if type(val)==dict:
                        for v in val:
                            if v == '$exists':  #query = {'sample_id': {'$exists': 1}}
                                if val[v]==1 and key not in row:
                                    break
                                elif val[v]==0 and key in row:
                                    break
                            elif v == '$in':    #query = {id_field: {"$in": ids}}
                                if row[key] not in val[v]:
                                    break
                        else:
                            rows.append(row)
                            row_nb.append(index)
                        break             
                    elif key not in row or row[key] != val:
                        break
                            
                else:
                    rows.append(row)
                    row_nb.append(index)
        if len(rows) == 0:
            return []

        included=[]
        id_excluded = False
        if filter == None:filter={}
        for f in filter:
            if filter[f]==0:#explicitely excluded
                if f == "_id": id_excluded = True
                for row in rows: 
                    if f in row: del row[f]
            elif filter[f]==1:#explicitely included so everything else is excluded except possibly _id 
                included.append(f)
            
        if len(included) != 0:
            for row in rows:
                for k in list(row.keys()):
                    if k not in included and k != "_id":
                        del row[k]
                    if k == "_id" and id_excluded:
                        del row[k]
        
        return rows
        
    def load_data(self):
            
        if os.path.isfile(self.filename):
            print(f"loading data {self.filename}")
            
            with open(self.filename) as f:
                self.data = json.load(f)
                
            #with open(self.filename) as f:
            #    content = f.read()
            #    if content[:2]=='[[':
            #        self.data = self.data[0]
        else:
            self.data = []
                    
        
    def find_one(self, query={}, filter={}):
        """
        finds first row in the collection data corresponding to query and limited to fields passed in filter
        Parameter:
        query: dict that contains the query
        filter: dict that lays out the fields to include/exclude and also handles $in/$exists functions
        Return:
        The dict corresponding to the first row found
        """
        
        self.load_data()
        
        rows = self.find(query, filter)
        if len(rows)==0:
            return None
        else:
            return(rows[0])
    
    def insert_one(self, json_load_content:dict):
        """
        inserts the passed dict as a new row
        adds process id to created id to make it unique when multiprocessing involved
        Parameter:
        json_load_content: dict that contains the data
        Return:
        Nothing
        """
        
        json_load_content["_id"] = str(os.getpid()) + str(len(self.data))
        self.data.append(json_load_content)
        self.save()
        class result:
            def __init__(self, inserted_id):
              self.inserted_id = inserted_id
        results = result(json_load_content["_id"])  
        return results
        
    def insert_many(self, json_load_content:list):
        """
        inserts the passed list of dict as new rows
        adds process id to created id to make it unique when multiprocessing involved
        Parameter:
        json_load_content: list of dict that contains the data
        Return:
        result object with attribute inserted_ids
        """

        for item in json_load_content:
            item["_id"] = str(os.getpid()) + str(len(self.data))
            self.data.append(item)
        self.save()
        class result:
            def __init__(self, inserted_ids):
              self.inserted_ids = inserted_ids
        results = result(self.data[-len(json_load_content):])  
        return results
        
    def delete_many(self, query={}):
        """
        deletes rows corresponding to the passed query
        Parameter:
        the query to use
        Return:
        result object with attribute deleted_count
        """
        
        row_nb=[]
        items = self.find(query, {}, row_nb)
        for r in row_nb[::-1]:
            del self.data[r]
        
        self.save()
        class result:
            def __init__(self, deleted_count):
                self.deleted_count = deleted_count
        results = result(len(items))
        return results
        
        
    def delete_one(self, query={}):
        """
        deletes first found row corresponding to the passed query
        Parameter:
        the query to use
        Return:
        Nothing
        """
        
        row_nb=[]
        items = self.find(query, {}, row_nb)
        if len(row_nb) >0:
            del self.data[row_nb[0]]
        self.save()
        
    def update_one(self, query, update):
        """
        update first found row corresponding to the passed query with values passed in update dict
        Parameter:
        the query to use: dict  ex: #{'sample_id': k}
        update: the dict corresponding to the changes ex: #{'$set': {'qc_flag': 'LOW_NSIG80'}})
        Return:
        Nothing
        """
        
        row_nb=[]
        items = self.find(query, {}, row_nb)
        if len(row_nb) >0:
            for field in update["$set"]:
                self.data[row_nb[0]][field] = update["$set"][field]
                
        self.save()
        
    def count_documents(self, query={},filter={}):
        """
        counts rows corresponding to passed query
        Parameter:
        query: dict
        filter: dict
        """
        
        rows = self.find(query,filter)
        return len(rows)
        
    def count_documents(self, query={},filter={}):
        """
        counts rows corresponding to passed query
        Parameter:
        query: dict
        filter: dict
        """
        
        rows = self.find(query,filter)
        return len(rows)
        
    def distinct(self, field, query={}):
        """
        finds rows corresponding to query and returns unique values of passed field
        Parameter: 
        field: str
        query: dict
        Return:
        list of unique values
        """
        
        rows = self.find(query=query,filter={field:1})
        d = set()
        for r in rows: 
            d.add(r[field])
        return list(d)
    


