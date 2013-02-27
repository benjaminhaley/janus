# -*- coding: utf-8 -*-
import urllib2
from re import findall, match, sub
from os.path import exists
from os import makedirs, listdir
from time import sleep
from random import randint

"""
# Configure Authentication

username = ???
password = ???
top_level_url = 'https://era.bfs.de'

password_mgr = urllib2.HTTPPasswordMgrWithDefaultRealm()
password_mgr.add_password(None, top_level_url, username, password)
urllib2.install_opener(urllib2.build_opener(urllib2.HTTPBasicAuthHandler(password_mgr)))


# Get study ids
#
# We will need the LabId's and StudyId's to generate the csv urls
# The browse page has all of these in its source

url = 'https://era.bfs.de/studies_search.php?stage=3&type=browse'
pattern = r'studies_details.php.LabId=([0-9]*).StudyId.([0-9]*)'
page = urllib2.urlopen(url).read()
ids = findall(pattern, page)


# Get data
#
# csvs will be placed in a temporary directory

dir_ = "/Users/benjaminhaley/temp/"
base = "https://era.bfs.de/studies_export.php?"
if not exists(dir_): makedirs(dir_)
for labid, studyid in ids[40:]:
    try:
        url = base + "LabId=" + labid + "&StudyId=" + studyid
        name = 'lab_' + labid + '_study_' + studyid + '.csv'
        file(dir_ + name, 'w').write(urllib2.urlopen(url).read())
        print name + " written"
    except:
        print name + "NOT WRITTEN!!!!!"
        raise
    
    sleep(randint(1, 10))
"""



# define rules
def remove_stray_newlines(lines):
    """Sometimes a line is split unnaturally.
    This can be detected because the line starts with a | character
    this function detects those instances and merges the line with the previous one
    """
    partial = [l.startswith('|') for l in lines]
    lines = [
        prev + l if part else prev
        for prev, l, part in zip(lines, lines[1:], partial[1:])
    ] + [lines[-1]]
    lines = [l for l, part in zip(lines, partial) if not part]

    return lines


def expand_treatments(lines):
    """Sometimes treatments come in a single cell
    and they must be expanded by turning commas to
    seperators, but only in that cell"""
    
    p = r'^Sequence:.*Remarks:.*'
    p2 = r':([^:]*)(?:,|$)'
    
    return [
        '|'.join([
            '|'.join(findall(p2, c))
            if match(p, c) else c
            for c in l.split('|')
        ])
        for l in lines
    ]

def strip_all(lines):
    return ['|'.join([c.strip() for c in l.split('|')]) for l in lines]

def merge_unix_line_endings(lines):
    """\r\n line endings are a mistake and need to be expelled"""
    return [l + '\n' for l in ''.join(lines).replace('\r\n', '').split('\n')[:-1]]

def clean_2_12(lines):
    """lab_2_study_12.csv has this weird intergected string about silene followed
    by a newline.  I think the best solution is to simply remove all instances"""
    p = r'(?i)(silene|Cf252)[^|\n]*\n'
    return [l + '\n' for l in sub(p, '', ''.join(lines)).split('\n')[:-1]]

def ensure_valid_groups(lines):
    """valid group ids have the form like 3-22-46 or 31-2-19 if a row starts with something
    else then it is likely that it was cutoff from teh line above and should be merged with
    it"""

    p = r'^[0-9]{1,5}-[0-9]{1,5}-[0-9]{1,5}'

    partial = [not match(p, l) for l in lines]
    lines = [
        prev + l if part else prev
        for prev, l, part in zip(lines, lines[1:], partial[1:])
    ] + [lines[-1]]
    lines = [l for l, part in zip(lines, partial) if not part]

    return lines
    

def test():
    assert(remove_stray_newlines(["A","B"]) == ["A", "B"])
    assert(remove_stray_newlines(["A","|B"]) == ["A|B"])
    assert(expand_treatments(["A|B"]) == ["A|B"])
    assert(expand_treatments(["A|...|...|...|,|"]) == ["A|...|...|...|,|"])
    assert(expand_treatments(["Sequence:1, Age:6, Remarks:aaah"]) == ["1|6|aaah"])
    assert(expand_treatments(["A|...|Sequence:1,Remarks:aaah|...|,|"]) == ["A|...|1|aaah|...|,|"])
    assert(expand_treatments(["Sequence:1, Application:Injection, intravenous, Remarks:..."]) == ["1|Injection, intravenous|..."])
    assert(strip_all(['| 77|']) == ['|77|'])
    assert(merge_unix_line_endings(["news \r\n", "story\n"]) == ["news story\n"])
    assert(clean_2_12(["Very Silene 0.6Gy males 21.6.1989  75151-75174\n", "Cool\n"]) == ["Very Cool\n"])
    assert(clean_2_12(["Very SILENE 0,6 Gy  3.9.91 50  77501-77550\n", "Cool\n"]) == ["Very Cool\n"])
    assert(clean_2_12(["Very Cf252 20.6.91 10.3.91 vie relle 50 1-50 2\n", "Cool\n"]) == ["Very Cool\n"])
    assert(ensure_valid_groups(["1-19-32", " two", "1-19-3"]) == ["1-19-32 two", "1-19-3"])

test()




#"""

# clean
dirty_dir = "/Users/benjaminhaley/janus/scripts/data/era_scrape/"
clean_dir = "/Users/benjaminhaley/janus/scripts/data/era/"
files     = listdir(dirty_dir)

big = []
big_file = file(clean_dir + 'big.csv', 'w')
big_file.writelines('file|'+open(dirty_dir + files[0]).readlines()[0])

for fh in files:
        
    # read
    lines = open(dirty_dir + fh).readlines()

    # degub
    print len(lines),

    # apply rules
    lines = merge_unix_line_endings(lines)
    lines = clean_2_12(lines)
    lines = [l.rstrip() for l in lines]
    lines = remove_stray_newlines(lines)
    lines = remove_stray_newlines(lines)   # for good measure
    lines = expand_treatments(lines)
    lines = strip_all(lines)
    lines[1:] = ensure_valid_groups(lines[1:])
    lines = [l + '\r\n' for l in lines]

    # degub
    print len(lines), fh

    # write to file
    file(clean_dir + fh, 'w').writelines(lines)
    big_file.writelines([fh+'|'+l for l in lines[1:]])


#"""
