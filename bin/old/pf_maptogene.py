#!/usr/bin/env python3
from argparse import ArgumentParser
import os


def read_map_file(file_path):
    map_dict = {}
    with open(file_path, 'r') as fp:
        for index, line in enumerate(fp):
            if line.startswith('#'):
                continue
            info = line.strip().split('\t')
            map_id, map_attr = info[0], info[1]
            map_dict.setdefault(map_id, map_attr)
    if len(map_dict) == 0:
        return None
    else:
        return map_dict


def reade2p2(e2p2_output):
    e2p2_id, e2p2_attr = "", ""
    for line in e2p2_output:
        if line.startswith("ID\t"):
            if len(e2p2_id) > 0:
                yield e2p2_id, e2p2_attr
            e2p2_id, e2p2_attr = line, ""
        else:
            e2p2_attr += line
    if len(e2p2_id) > 0:
        yield e2p2_id, e2p2_attr


def mapprotein_to_gene(file_input, prot_gene_map):
    output_path = os.path.splitext(file_input)[0] + '.revised.pf'
    with open(file_input, 'r') as fp, open(output_path, 'w') as op:
        for e2p2_id, e2p2_attr in reade2p2(fp):
            id_to_write = 0
            unique_id = e2p2_id.replace('ID', '').replace('\\', '').strip()
            try:
                mapped_id = prot_gene_map[unique_id]
            except KeyError:
                print(unique_id, 'ID not found in map.')
                continue
            info = e2p2_attr.strip().replace('\n//', '').split('\n')
            for attr_line in info:
                attr = [a.strip() for a in attr_line.split('\t')]
                if attr[0] == "NAME":
                    try:
                        op.write('ID\t' + prot_gene_map[unique_id] + '\n')
                        op.write('NAME\t' + prot_gene_map[attr[1]] + '\n')
                        op.write('PRODUCT-ACCESSION\t' + attr[1] + '\n')
                        id_to_write = 1
                        continue
                    except KeyError:
                        print(attr[1], "NAME not found in map")
                        id_to_write = 0
                        break
                if id_to_write == 1:
                    op.write(attr_line + '\n')
            if id_to_write == 1:
                op.write('//\n')


def main():
    parser = ArgumentParser()
    parser.add_argument("file_input")
    parser.add_argument("prot_gene_map_path")

    args = parser.parse_args()
    mapprotein_to_gene(args.file_input, read_map_file(args.prot_gene_map_path))


if __name__ == '__main__':
    main()
