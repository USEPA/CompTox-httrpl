import os
import sys

# Setup path to lib/ - assumes this script is in bin/ subdir next to lib/
TOP = '/'.join(os.path.abspath(__file__).split('/')[0:-2])
TOP = TOP + '/'
LIB = TOP + 'lib'
if LIB not in sys.path:
    sys.path.insert(0, LIB)
os.environ['PYTHONPATH'] = LIB

from gexp.biospyder2 import biospider2
from httrplcore import PipelineLogger, cleanPath, deflog
from align_and_count import align_and_count
from gexp.fastq import fastqGC

class update_gc_frac_multiproc(biospider2):
    """the functionality found in biospider such as multiprocessing can be used and refined by creating a subclass and 
    overwriting the fastqRawInfo method to tailor for our specific need
    The rest of the logic in the function from biospider2 are fine as they are.
    """
    
    def fastqRawInfo(self, fq_file: str, sample_id: str = None,
                     log: PipelineLogger = deflog) -> dict:
        res=  fastqGC(fq_file)
        res['sample_id'] = sample_id
        return res

class update_gc_frac(align_and_count):
    """ this class is subclass of align_and_count to leverage its functionality such batch processing and general logic"""
    def __init__(self, config=None):
      super().__init__(config)
      self.biospider = update_gc_frac_multiproc()
      
    def process_gc_frac(self):
    
        """
        process_gc_frac looks up all documents in the httr_raw collection and for each sample_id/fastq file calculates the ratio of Gs
        and Cs by using the fastqGC function and writes that info into the gc_frac column.
        It uses mutliprocessing and follow the batch size as specified in the config.json file
        If rerun is set to false, it only updates documents that lacks a gc_frac column
        
        Parameter:
        None
        Return:
        None

        """

        rerun, status = self.prepare_processing()
        if status == False: return

        fq_files = list(self.DB[self.config['db_collection_raw']].find({}))
        if rerun==False:
            fq_files = [e for e in fq_files if 'gc_frac' not in e]
        for e in fq_files: e['fq_file'] = e['path'] + "//" + e['fastq']
        
        if len(fq_files)==0: self.log.write(f"no document found whose gc_frac needs to be updated",timestamp=True)
        
        offset = 1 if len(fq_files) % self.batch_size != 0 else 0
        for e in range(len(fq_files) // self.batch_size + offset):

            fastqGC_values = self.biospider.fastqRawInfoMulti(fq_files=fq_files[e * self.batch_size:min(
                len(fq_files), (e + 1) * self.batch_size)], p=self.config['p'], log=self.log)
                
            for index in range(e * self.batch_size,min(len(fq_files), (e + 1) * self.batch_size)):
                s_id = fq_files[index]['sample_id']
                fastqGC_value = fastqGC_values[index-e * self.batch_size]
                self.DB[self.config['db_collection_raw']].update_one({'sample_id':s_id},{'$set':{'gc_frac':fastqGC_value['sample_gc']}})
   
            self.log.write(f"Amended {len(fastqGC_values)} documents gc_frac field into {self.config['db_collection_raw']} collection.", timestamp=True)
            self.log.write('Task completed successfully!', timestamp=True)
  