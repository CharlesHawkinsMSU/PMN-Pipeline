import os
from argparse import ArgumentParser

makeblastdb_cmd = 'makeblastdb -dbtype prot -parse_seqids -in $cycfasta -title "$cycname $cycversion"'



def read_to_create(fp):
    for line in fp:
        if line.startswith('#'):
            continue
        info = line.split('\t')
        try:
            pgdb_name = info[1].strip()
            version = info[9].strip()
            yield pgdb_name, version
        except IndexError:
            print('Field number error', line)


def generate_mkblastdb_script(to_create_path, fasta_folder, output_path):
    with open(to_create_path, 'r') as fp, open(output_path, 'w') as op:
        for pgdb_name, version in read_to_create(fp):
            fasta_path = os.path.join(fasta_folder, pgdb_name.lower() + '.fasta')
            # print(fasta_path)
            if not os.path.isfile(fasta_path):
                print('Fasta Not Found', os.path.basename(fasta_path))
                continue

            if version == '':
                version = 1.0
            else:
                version = float(version)

            cmd_to_write = makeblastdb_cmd.replace('$cycfasta', fasta_path).replace('$cycname', pgdb_name).replace('$cycversion', str(version))
            op.write(cmd_to_write + '\n')


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('to_create')
    parser.add_argument('fasta_folder')
    parser.add_argument('output_path')

    args = parser.parse_args()

    generate_mkblastdb_script(args.to_create, args.fasta_folder, args.output_path)




