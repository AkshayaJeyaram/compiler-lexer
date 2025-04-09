import re
import sys
cfp_file = "cfp.txt"
istemp = lambda s : bool(re.match(r"^t[0-9]*$", s))
isid = lambda s : bool(re.match(r"^[A-Za-z][A-Za-z0-9_]*$", s))

binary_operators = {"+", "-", "*", "/", "*", "&", "|", "^", ">>", "<<", "==", ">=", "<=", "!=", ">", "<","&&","||"}

o = open("dce.txt","w")

def remove_dead_code(list_of_lines) :
#Temporaries that are never assigned to any variable nor used in any expression are deleted. Done recursively.
	num_lines = len(list_of_lines)
	temps_on_lhs = set()
	for line in list_of_lines :
		tokens = line.split()
		if istemp(tokens[0]) :
			temps_on_lhs.add(tokens[0])

	useful_temps = set()
	for line in list_of_lines :
		tokens = line.split()

		if len(tokens) >= 3 :
			if istemp(tokens[2]) :
				useful_temps.add(tokens[2])
			else:
				rhs=tokens[2:]
				for i in rhs:
					if istemp(i):
						useful_temps.add(i)

	temps_to_remove = temps_on_lhs - useful_temps
	new_list_of_lines = []
	for line in list_of_lines :
		tokens = line.split()
		if tokens[0] not in temps_to_remove :
			new_list_of_lines.append(line)
	return new_list_of_lines

if __name__ == "__main__" :
	list_of_lines = []
	f = open(cfp_file, "r")
	for line in f :
		list_of_lines.append(line)
	f.close()
	without_deadcode = remove_dead_code(list_of_lines)
	for line in without_deadcode :
		o.write(line.strip()+"\n")
	o.write("\n\nEliminated "+str(len(list_of_lines)- len(without_deadcode))+" lines of code")
	print("Eliminated "+str(len(list_of_lines)- len(without_deadcode))+" lines of code")
	o.close()
