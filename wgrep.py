#!/usr/bin/python

# wgrep.py: Copyright Lukas Lazarek 2018
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import sys
import httplib2
import itertools as it
from BeautifulSoup import BeautifulSoup, SoupStrainer
import urlparse
import functools as ft
import re
import argparse


# main: -> Int
def main():
    args = parseArgs()

    if not (args.link and args.content):
        print("Missing mandatory argument(s). Try -h for help.")
        return 1

    wgrep(args.link, args.urls, args.content, args.negate, args.depth,
          displayResult=True)
    return 0



# isValidLink: Link -> Bool
def isValidLink(link):
    return link.startswith("http")

# domainOf: Link -> String
def domainOf(link):
    return '{uri.scheme}://{uri.netloc}/'.format(uri=urlparse.urlparse(link))

# normalizeRelative: Link/Domain Link/Relative -> Link
def normalizeRelative(domain, relative_link):
    return urlparse.urljoin(domain, relative_link)

# getLinksOn: HtmlSoup -> Iterable[Link/Relative]
def getLinksOn(htmlSoup):
    for link in htmlSoup.findAll('a'):
        if link.has_key('href'):
            yield link['href']

# relevantLinks: HtmlSoup Link Regexp -> Iterable[Link]
def relevantLinks(htmlSoup, link, pat):
    makeAbsolute = ft.partial(normalizeRelative, domainOf(link))
    matchesPat = ft.partial(re.search, pat)
    return it.ifilter(matchesPat, it.imap(makeAbsolute, getLinksOn(htmlSoup)))

# search: HtmlSoup Regexp -> Iterable[re.Match]
def search(htmlSoup, pat):
    igrep = lambda s: re.findall(".*" + pat + ".*", s, re.IGNORECASE)
    return igrep(htmlSoup.getText("\n"))

# flatMap: (A -> Iterable[B]) Iterable[A] -> Iterable[B]
def flatMap(f, l):
    return it.chain.from_iterable(it.imap(f, l))

# unique: Iterable[A] [(A -> B)] -> Iterable[A]
# Note: Stable
#
# ELEMENT_TRANSFORMER, if given, transforms each element before
# checking uniqueness against all other transformed elements
# E.g.: unique('ABBCcAD', str.lower) --> ['A', 'B', 'C', 'D']
def unique(iterable, element_transformer=None):
    seen = set()
    if element_transformer is None:
        for element in it.ifilterfalse(seen.__contains__, iterable):
            seen.add(element)
            yield element
    else:
        for element in iterable:
            k = element_transformer(element)
            if k not in seen:
                seen.add(k)
                yield element

# visit: Link Http Regexp Regexp Bool Int [Int=0 Bool=True]
#        ->
#        List[(Link, List[re.Match])]
def visit(link, http, urlPat, contentPat, negate, maxDepth,
          depth = 0, displayResult = True):
    if depth <= maxDepth:
        try:
            status, response = http.request(link)
        except:
            response = ""
        htmlSoup = BeautifulSoup(response)
        
        contentMatches = search(htmlSoup, contentPat)
        includeMatch = (contentMatches     and not negate) or \
                       (not contentMatches and negate)
        thisResult = [(link, contentMatches)] if includeMatch else []
        if displayResult and thisResult:
            display(thisResult[0])

        linksToFollow = unique(relevantLinks(htmlSoup, link, urlPat))
        childrenResults = list(flatMap(ft.partial(visit,
                                                  http=http,
                                                  urlPat=urlPat,
                                                  contentPat=contentPat,
                                                  negate=negate,
                                                  maxDepth=maxDepth,
                                                  depth=(depth+1),
                                                  displayResult=displayResult),
                                       linksToFollow))

        return thisResult + childrenResults

    else:
        return []

# wgrep: Link Http Regexp Regexp Bool Int [Int=0 Bool=True]
#        ->
#        List[(Link, List[re.Match])]
def wgrep(link, urlPat, contentPat, negate, maxDepth,
          depth = 0, displayResult = True):
    return visit(link, httplib2.Http(),
                 urlPat, contentPat, negate,
                 maxDepth, depth,
                 displayResult=displayResult)

# display: (Link, List[re.Match]) -> None
def display(wgrep_result):
    print("\n== " + wgrep_result[0] + " ==")
    for line in wgrep_result[1]:
        print("> "+ line)
    print("\n")



# parseArgs: -> Args
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



if __name__ == '__main__':
    sys.exit(main())
