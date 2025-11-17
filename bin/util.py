# General utility functions not specific to PMN

# Given a list (or other iterable) of strings (or objects that are convertible to strings), returns an English-style list of them; e.g. "Alice, Bob, and Carol" is returned for andlist(["Alice", "Bob", "Carol"]).
#  <oxford> turns on or off the Oxford comma (a comma after the penultimate element when there are at least three elements)
#  <copula> is the copula to use (usually this will be "and" or "or". Default is "and")
#  <quote> can be used to put quotes or brackets around the list items and should be a string of one or two characters (depending on whether you want different start and end quote characters), so for example '"' will put quote marks or '()' will put parens. You can also give quote = True as a synonym for '"', to make it easier to use in e.g. f-strings
#  Examples:
#   andlist(['Hello', 'Goodbye']) -> "Hello and Goodbye"
#   andlist(['This', 'That', 'T\'other']) -> "This, That, and T'Other"
#   andlist(['This', 'That', 'T\'other'], oxford = False) -> "This, That and T'Other"
#   andlist(['To be', 'not to be'], copula = 'or') -> "To be or not to be"
#   andlist(['a','b','c'], quote='"') -> '"a", "b", and "c"'
#   andlist(['a','b','c'], quote='[]') -> '[a], [b], and [c]'
def andlist(l, oxford = True, copula = 'and', quote = None):
	if quote == True:
		quote = '"'
	l = [f'{quote[0]}{element}{quote[-1]}' for element in l] if quote else list(l)
	match len(l):
		case 0: 
			return ''
		case 1:
			return f'{l[0]}'
		case 2:
			return f'{l[0]} {copula} {l[1]}'
		case _:
			return f'{", ".join(l[:-1])}{"," if oxford else ""} {copula} {l[-1]}'

# Convert the input value to a list. None returns an empty list. Iterables other than strings return themselves converted to a list. Strings and non-iterables return themselves put into a one-item list
def as_list(val):
	if val is None:
		return []
	elif isinstance(val, str):
		return [val]
	else:
		try:
			iter(val)
			return list(val)
		except TypeError:
			return [val]

def multi_range(instr):
	if type(instr) == int:
		yield instr
		return
	for range_spec in instr.split(','):
		start_end=range_spec.split('-')
		if len(start_end) == 1:
			yield int(range_spec)
		elif len(start_end) == 2:
			start = int(start_end[0])
			end = int(start_end[1])
			if start > end:
				warn(f'Requested range from {start} to {end} but {start} is greater than {end}; ignoring this range')
			for i in range(start, end+1):
				yield i
		else:
			raise ValueError(f'Invalid range: "range_spec"')

def pluralize(qty, singular, plural = None):
    'Returns the singluar or plural form of the word, based on qty. qty should be a number or a collection in which case its length is used. If qty is equal to 1 or is a collection of length 1, the singular form is used. Otherwise the plural form is used. If plural is not given, it is guessed based on the singular form using guess_plural()'
    try:
        qty = len(qty)
    except TypeError:
        pass
    if qty == 1:
        return singular
    else:
        if plural is None:
            return guess_plural(singular)
        else:
            return plural

def guess_plural(word):
    'Tries to guess the pluralization of the given word, based on its ending. If it ends in sh, ch, s, z, or x, it puts "es" at the end (box -> boxes). If it ends in y, it replaces the y with "ies" (try -> tries). In other cases it puts "s" at the end.'
    if word.endswith('s') or word.endswith('z') or word.endswith('x') or word.endswith('ch') or word.endswith('sh'):
        return word + 'es'
    if word.endswith('S') or word.endswith('Z') or word.endswith('X') or word.endswith('CH') or word.endswith('SH'):
        return word + 'ES'
    if word.endswith('y'):
        return word[:-1]+'ies'
    if word.endswith('Y'):
        return word[:-1]+'IES'
    if word and word[-1].isupper():
        return word + 'S'
    return word + 's'

    
