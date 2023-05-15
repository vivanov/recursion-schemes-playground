## Introduction to the recursion schemes in Scala

This repository is a Scala port of parts 1, 2 and (partially) 4 1/2 of the excellent `Introduction to the Recursion Schemes` series by Patrick Thomson (in Haskell):
  * site: https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html
  * repo: https://github.com/patrickt/recschemes

It also has small fixpoint example based on great article by Ziyang Liu `The fix Combinator in Scalaz` (implemented using `fix` combinator of the Cats library):
  * https://free.cofree.io/2017/08/28/fixpoint/
  * his blog has many other useful articles (including post about recursion schemes)

Note that this is a naive implementation of some base recursion schemes. For real-world, production-ready resursion schemes implementation in Scala see Droste library:
  * https://github.com/higherkindness/droste
