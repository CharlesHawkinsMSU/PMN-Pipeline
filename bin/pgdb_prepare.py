import pmn
import shutil
import os
from os import path
def pgdb_prepare (config, orgtable, orglist):
    for org in orglist:
        entry = orgtable[org]

        # create folder for org
        master_dir = path.join(config['proj-masters-dir'], org)
        os.makedirs(master_dir, exist_ok = True)
    
        # copy pf file to the folder
        shutil.copy(entry['PF File'], master_dir)
        pmn.info(f'Preparing for {org} (PGDB-UNIQUE-ID: {entry["Unique ID"]})...\n')

        # write create.master file
        create_master = open(path.join(master_dir, 'create.master'), 'w')
        create_master.write(config["ptools-exe"]+'\n')
        create_master.close()

        # write genetic-elements.dat file

        genetic_elements = open(path.join(master_dir, 'genetic-elements.dat'), 'w')
        id_name = entry['ID/Name']
        genetic_elements.write(f'ID\t{id_name}\nNAME\t{id_name}\nANNOT-FILE\t{path.basename(entry["PF File"])}\n//\n')
        genetic_elements.close()
        
        # write organism-params.dat file
        org_params = open(path.join(master_dir, 'organism-params.dat'), 'w')
        org_params.write(f'ID\t{org}\nSTORAGE\tFILE\nNAME\t{entry["Species Name"]}\nABBREV-NAME\t{entry["Abbrev Name"]}\nSEQUENCE-SOURCE\t{path.basename(entry["Sequence File"])}\nDOMAIN\t{entry["NCBI Taxon ID"]}\nRANK\t|species|\nCODON-TABLE\t1\nMITO-CODON-TABLE\t1\nHOMEPAGE\t{entry["Homepage"]}\nEMAIL\t{entry["Email"]}\nDBNAME\t{entry["Database Name"]}\nNCBI-TAXON-ID\t{entry["NCBI Taxon ID"]}\nREF-ORGID\t{entry["Reference DB"].upper()}\nORG-COUNTER\t{pmn.counter_from_id(entry["Unique ID"])}\n')
        org_params.close()
         
        # write dump.master foreach species
        dump = open(path.join(master_dir, 'dump.master'), 'w')
        dump.write(f's_ptools\t{config["ptools-exe"]}\nlisp\t(so \'{org})\nlisp\t(setq *file-export-progress* nil)\nlisp\t(dump-frames-to-attribute-value-files (org-data-dir))\nlisp\t(dump-frames-to-tabular-files (org-data-dir))\n')
        dump.close()
        
        # write dump-biopax.master foreach species
        dump_biopax = open(path.join(master_dir, 'dump-biopax.master'), 'w')
        dump_biopax.write(f's_ptools\t{config["ptools-exe"]}\nlisp\t(so \'{org})\nlisp\t(setq *file-export-progress* nil)\nlisp\t(com-export-pgdb-to-biopax)')
        dump_biopax.close()
