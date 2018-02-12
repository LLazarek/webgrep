#!/usr/bin/python

import sys
import httplib2
import itertools as it
from BeautifulSoup import BeautifulSoup, SoupStrainer
import urlparse
import functools as ft
import re
import argparse


def isValidLink(link):
    return link.startswith("http")

def domainOf(link):
    return '{uri.scheme}://{uri.netloc}/'.format(uri=urlparse.urlparse(link))

def normalizeRelative(domain, relative_link):
    return urlparse.urljoin(domain, relative_link)

def getLinksOn(htmlSoup):
    for link in htmlSoup.findAll('a'):
        if link.has_key('href'):
            yield link['href']

def relevantLinks(htmlSoup, link, pat):
    makeAbsolute = ft.partial(normalizeRelative, domainOf(link))
    matchesPat = ft.partial(re.search, pat)
    return it.ifilter(matchesPat, it.imap(makeAbsolute, getLinksOn(htmlSoup)))

def search(htmlSoup, pat):
    igrep = lambda s: re.findall(".*" + pat + ".*", s, re.IGNORECASE)
    return igrep(htmlSoup.getText("\n"))

def flatMap(f, l):
    return it.chain.from_iterable(it.imap(f, l))

def unique(iterable, key=None):
    "List unique elements, preserving order. Remember all elements ever seen."
    # unique_everseen('AAAABBBCCDAABBB') --> A B C D
    # unique_everseen('ABBCcAD', str.lower) --> A B C D
    seen = set()
    seen_add = seen.add
    if key is None:
        for element in it.ifilterfalse(seen.__contains__, iterable):
            seen_add(element)
            yield element
    else:
        for element in iterable:
            k = key(element)
            if k not in seen:
                seen_add(k)
                yield element

def visit(link, http, urlPat, contentPat, maxDepth, depth = 0):
    if depth <= maxDepth:
        try:
            status, response = http.request(link)
        except:
            response = ""
        htmlSoup = BeautifulSoup(response)
        
        contentMatches = search(htmlSoup, contentPat)
        thisResult = [(link, contentMatches)] if contentMatches else []

        linksToFollow = unique(relevantLinks(htmlSoup, link, urlPat))
        childrenResults = list(flatMap(ft.partial(visit,
                                                  http=http,
                                                  urlPat=urlPat,
                                                  contentPat=contentPat,
                                                  maxDepth=maxDepth,
                                                  depth=(depth+1)),
                                       linksToFollow))

        return thisResult + childrenResults
    else:
        return []

def visit_print(link, http, urlPat, contentPat, negate, maxDepth, depth = 0):
    if depth <= maxDepth:
        try:
            status, response = http.request(link)
        except:
            response = ""
        htmlSoup = BeautifulSoup(response)
        
        contentMatches = search(htmlSoup, contentPat)
        if contentMatches and not negate:
            display((link, contentMatches))
        elif not contentMatches and negate:
            print(link)

        linksToFollow = unique(relevantLinks(htmlSoup, link, urlPat))
        assert(linksToFollow is not None)
        for childLink in linksToFollow:
            visit_print(childLink, http, urlPat,
                        contentPat, negate,
                        maxDepth, depth + 1)

def wgrep(link, urlPat, contentPat, maxDepth, depth = 0):
    return visit(link, httplib2.Http(), urlPat, contentPat, maxDepth, depth)

def wgrep_print(link, urlPat, contentPat, negate, maxDepth, depth = 0):
    return visit_print(link, httplib2.Http(),
                       urlPat, contentPat, negate,
                       maxDepth, depth)

def display(wgrep_result):
    print("\n== " + wgrep_result[0] + " ==")
    for line in wgrep_result[1]:
        print("> "+ line)
    print("\n")

link = 'https://www.uml.edu/Research/labs.aspx'
urlPat = 'uml\\.edu'
contentPat = 'toxic'

def parseArgs():
    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--link", type=str,
                        help="The link to root the search. "
                        "*This is a mandatory argument.*")
    parser.add_argument("-u", "--urls", type=str, default=".*",
                        help="Regexp for urls to search. "
                        "By default all urls are searched.")
    parser.add_argument("-c", "--content", type=str,
                        help="Regexp for page content to find. "
                        "*This is a mandatory argument.*")
    parser.add_argument("-d", "--depth", type=int, default="2",
                        help="The page depth to search. "
                        "Default: 2.")
    parser.add_argument("-n", "--negate", action="store_true",
                        help="Negate content regexp: "
                        "Print only page urls that do *not* contain "
                        "the content pattern.")
    return parser.parse_args()

def main():
    args = parseArgs()

    if not (args.link and args.content):
        print("Missing mandatory argument(s). Try -h for help.")
        return 1

    wgrep_print(args.link, args.urls, args.content, args.negate, args.depth)
    return 0



if __name__ == '__main__':
    sys.exit(main())

# TODO
# - Look into pretty printing w/ colors
# - ? Add option for case sensitivity
# - ? Allow multiple patterns
# - make faster?
