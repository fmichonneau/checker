---
layout: post
title:  "Collection of links to test"
date:   2019-04-12 12:18:38 -0400
categories: checker tests
---

Here is a collection of functional links:

- [functional jekyll created link]({% link pages/cats.md %})
- [functional manual absolute](/pages/cats.html)
- [functional manual with full url]({{ site.url }}{{ site.baseurl }}/pages/cats.html)

Here is a collection of broken links:

- [manual absolute broken](/pages/dogs.html)
- [manual full url broken]({{ site.url }}{{ site.baseurl }}/pages/dogs.html)
