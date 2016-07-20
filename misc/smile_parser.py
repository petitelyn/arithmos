import csv
import re
import os
import operator
import shutil
#find the directories to read from and write to
rootdir = 'C:\\Users\\gardnega\\Desktop\\Final_Smile'
writedir = 'C:\\Users\\gardnega\\Desktop\\Edited_FINAL'
#build write directory
os.makedirs(writedir)
#build a dictionary of all subjects to their groups
subject_to_group = {}
#loop through all directories
for subdir, dirs, files in os.walk(rootdir):
	#make a dictionary of variables to their respective units for all files 
	units = {}
	#loop through all files
	for file in files:
		#open the current csv, and open the location to write to
		cur_csv = open(rootdir + '\\' + file, "r+")
		to_write = open(writedir + '\\' + file, "w+")
		csv_dict = csv.DictReader(cur_csv)
		#loop through all column names
		for i in range(0, len(csv_dict.fieldnames)):
			name = csv_dict.fieldnames[i]
			#find the units between the parentheses in the name
			unit = name[name.find("(") + 1:name.find(")")]
			#build the name once the units are removed
			feature_name = csv_dict.fieldnames[i].replace('('+unit+')', '').strip()
			#timepoint, although it has (Visit), does not have units
			if feature_name == 'Timepoint':
				continue
			#if no parentheses, ignore the field (ie Subject#, Group)
			if name.find("(") == -1:
				units[csv_dict.fieldnames[i]] = ''
				continue
			#add percentage to variable name if unit is %
			if unit == '%':
				feature_name = '%' + feature_name
			#remove unit from column name
			csv_dict.fieldnames[i] = feature_name
			#record unit
			units[csv_dict.fieldnames[i]] = unit
		#add a timepoint column if not there
		if not 'Timepoint (Visit)' in csv_dict.fieldnames:
			csv_dict.fieldnames = csv_dict.fieldnames + ['Timepoint (Visit)']
		#add a group column if not there
		if not 'Group' in csv_dict.fieldnames:
			csv_dict.fieldnames = csv_dict.fieldnames + ['Group']	
		#get optimal column order
		writenames = "Subject#,Group,Timepoint (Visit)"
		for name in csv_dict.fieldnames:
			#rearrange columns, ignoring those already ordered above
			if name in ["Subject#", "Timepoint (Visit)", "Group", "Useless", "Remarks", "Obs"]:
				continue
			writenames = writenames + ',' + name  
		#get ready to write csv
		writer = csv.DictWriter(to_write, fieldnames = writenames.split(','), lineterminator = "\n", extrasaction='ignore')
		#write column names
		writer.writeheader()
		#loop through every row
		for row in csv_dict:
			#cut off if no more subjects
			if not "Subject#" in row or row['Subject#'] == '':
				break
			#if no group for subject, must add it
			if row['Group'] == None:
				#subject_to_group should already be built by uploading the files with groups first
				if row['Subject#'] not in subject_to_group:
					row['Group'] = 'NA'
				else:
					#record the group a subject belongs to
					row['Group'] = subject_to_group[row['Subject#']]
			else:
				#add the subject and group to the subject_to_group dictionary
				subject_to_group[row['Subject#']] = row['Group']
			#if there is no timepoint for a row, give it a static timepoint
			if row['Timepoint (Visit)'] == None:
				row['Timepoint (Visit)'] = -1
			#if timepoint row has like V1 or V2 in it, remove all digits so it is a single visit number
			if isinstance(row['Timepoint (Visit)'], str):
				row['Timepoint (Visit)'] = re.sub("\D", "", row['Timepoint (Visit)'])
			#write the row
			writer.writerow(row)
		#write the units at the bottom to be moved later
		writer.writerow(units)
		to_write.close()
		cur_csv.close()
	
	
