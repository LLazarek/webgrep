#!/bin/python

import httplib2
import itertools as it
from BeautifulSoup import BeautifulSoup, SoupStrainer
import urlparse
import functools as ft
import re


def isValidLink(link):
    return link.startswith("http")

def domainOf(link):
    return '{uri.scheme}://{uri.netloc}/'.format(uri=urlparse.urlparse(link))

def normalizeRelative(domain, relative_link):
    return urlparse.urljoin(domain, relative_link)

def getTextOf(page_link):
    print("Getting text of " + page_link)
    try:
        http = httplib2.Http()
        status, response = http.request(page_link)

        return BeautifulSoup(response).getText("\n")
    except:
        return ""

def getLinksOn(page_link):
    print("Grabbing links from " + page_link)
    try:
        http = httplib2.Http()
        status, response = http.request(page_link)

        for link in BeautifulSoup(response, parseOnlyThese=SoupStrainer('a')):
            if link.has_key('href'):
                yield link['href']
    except:
        pass

link = 'https://www.uml.edu/Research/labs.aspx'
urlPat = 'uml\\.edu'
contentPat = 'toxic'

# https://www.uml.edu/Research/labs.aspx
# https://en.wikipedia.org/wiki/Viva_Radio_2

def relevantLinks(link, pat):
    makeAbsolute = ft.partial(normalizeRelative, domainOf(link))
    matchesPat = ft.partial(re.search, pat)
    return it.ifilter(matchesPat, it.imap(makeAbsolute, getLinksOn(link)))

def search(link, pat):
    igrep = lambda s: re.findall(".*" + pat + ".*", s, re.IGNORECASE)
    return igrep(getTextOf(link))

def flatMap(f, l):
    return it.chain.from_iterable(it.imap(f, l))

def wgrep(link, urlPat, contentPat, maxDepth, depth = 0):
    if depth <= maxDepth:
        contentMatches = search(link, contentPat)
        linksToFollow = relevantLinks(link, urlPat)
        childrenResults = list(flatMap(ft.partial(wgrep,
                                                  urlPat=urlPat,
                                                  contentPat=contentPat,
                                                  maxDepth=maxDepth,
                                                  depth=(depth+1)),
                                       linksToFollow))
        if contentMatches:
            childrenResults.append((link, contentMatches))
        return childrenResults
    else:
        return []
