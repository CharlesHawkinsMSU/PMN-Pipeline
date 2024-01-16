#!/usr/bin/env python3

from sys import stder
from os import path
import threads
import subprocess

if __name__ == "__main__":
	stderr.write('This is a library PMN uses for interacting with the SLURM job management system. It does nothing when run by itself. To run the PMN pipeline, use the pmn-pipeline script')
	exit(1)

def run_job_file(job_file, array = None):
	cmd = ['sbatch', '--wait']
	if array:
		cmd += ['--array', array]
	cmd += job_file
	subprocess.run(cmd)

class SlurmJobArray:
	def __init__(self, task_list, config, name = 'PMN'):
		self.task_list = task_list
		self.job_list = []
		self.name = name
		self.tasks_per_job = config['slurm-job-pack']
		self.slurm_dir = config['slurm-tmp-dir']
		i = 0
		while i < len(task_list):
			tasks_for_job = task_list[i:i+self.tasks_per_job]
			self.job_list.append(tasks_for_job)
			i += self.tasks_per_job
		self.need_last_job = (len(self.job_list[-1]) < self.tasks_per_job)
	def write_job_files(self):
		conf_prefix = path.join(self.slurm_dir, self.name+'.job-')
		self.job_file = path.join(self.slurm_dir, self.name+'.slurm.sh')
		job_script = f'''#!/bin/bash
#SBATCH -J {self.name}
#SBATCH --partition DPB
#SBATCH --nodes=1
#SBATCH --ntasks={self.tasks_per_job}
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5000
#SBATCH --time=1000:00:00

'''
		try:
			job_script += f'module load {config["module-load"]}\n\n'
		except KeyError:
			pass
		job_script += f'srun --multi-prog {conf_prefix}$SLURM_ARRAY_TASK_ID.conf\n'
		with open(self.job_file, 'w') as job_file_stream:
			job_file_stream.write(job_script)
		if self.need_last_job:
			self.last_job_file = path.join(self.slurm_dir, self.name+'-last.slurm.sh')
			last_job_script = f'''#!/bin/bash
#SBATCH -J {self.name}
#SBATCH --partition DPB
#SBATCH --nodes=1
#SBATCH --ntasks={len(self.task_list)%len(self.tasks_per_job)}
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5000
#SBATCH --time=1000:00:00

'''
			try:
				last_job_script += f'module load {config["module-load"]}\n\n'
			except KeyError:
				pass
			last_job_script += f'srun --multi-prog {conf_prefix}-last.conf\n'
			with open(self.last_job_file, 'w') as last_job_file_stream:
				last_job_file_stream.write(last_job_script)
		for i in range(len(self.jobs_list)):
			job_conf_path = f'{conf_prefix}{i}.conf'
			with open(job_conf_path, 'w') as job_conf_file:
				for j in range(len(self.job_list[i])):
					job_conf_file.write(f'{j} {self.job_list[i][j]}\n')
	def run_all_jobs(self):
		if self.need_last_job:
			last_job_thread = threading.Thread(target = run_job_file, args = [self.jast_job_file])
			last_job_thread.run()
			run_job_file(self.job_file, array = f'0-{len(self.job_list)-2}')
			last_job_thread.join()
		else:
			run_job_file(self.job_file, array = f'0-{len(self.job_list)-1}')
