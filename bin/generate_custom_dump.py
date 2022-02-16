import time
import os
from argparse import ArgumentParser


def read_to_create(file_path):
    with open(file_path, 'r') as tcp:
        for line in tcp:
            info = line.split('\t')
            try:
                species_name = info[0].strip()
                pgdb_name = info[1].lower().strip()
                yield species_name, pgdb_name
            except IndexError:
                yield None, None


def generate_custom_dump(to_create_path, script_path, perl_script_path, output_dir):
    cur_time = time.strftime("%Y%m%d")
    with open(script_path, 'w') as sp:
        for species_name, pgdb_name in read_to_create(to_create_path):
            if None not in (species_name, pgdb_name):
                #i.e. aracyc_compounds.20160601 and aracyc_pathways.20160601
                if 'compound' in perl_script_path:
                    output_path = os.path.join(output_dir, pgdb_name + '_compounds.' + cur_time)
                elif 'pathway' in perl_script_path:
                    output_path = os.path.join(output_dir, pgdb_name + '_pathways.' + cur_time)
                else:
                    output_path = None
                    print('Perl Script Error')
                    exit()
                if output_path is not None:
                    bash_line = ' '.join(['perl', perl_script_path, species_name, '> ' + output_path])
                    sp.write(bash_line + '\n')


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('to_create_path')
    parser.add_argument('perl_script_path')
    parser.add_argument('output_dir')
    parser.add_argument('output_script_path')

    args = parser.parse_args()
    generate_custom_dump(os.path.abspath(args.to_create_path), os.path.abspath(args.output_script_path),
                         os.path.abspath(args.perl_script_path), os.path.abspath(args.output_dir))




