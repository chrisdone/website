---
date: 2008-12-18
title: Lojbot
description: Lojbot
author: Chris Done
tags: haskell
---

I don't think I have blogged at all about lojbot. Lojbot, or {lojbot}[^1], is an IRC bot[^2] which I have written, in Haskell, which runs on the Freenode IRC network, idles in #lojban and #lojbot. It has some neat commands which are useful to Lojbanists. I'm going to demonstrate some of its features briefly.

I've recently done over lojbot's command and tidied up the source. It's much better now.

I've marked this post as Lojban and Haskell, but it's only vaguely related to Haskell in that I wrote the bot and all the Lojban library stuff in it. I won't be discussing Haskell in this post.

## Help

First of all you can invoke the help command to list all the other commands, and you can invoke help for each of those commands. Commands, in channels, are prefixed with things, e.g. in #lojbot, it is ?command. In private message, however, no prefix is needed.

```
<chrisdone> help
<lojbot> commands: search (query, q), valsi (v), rafsi (r), gloss
(g), definition (d), selma'o (s), translate (t), selrafsi (sr), cll,
lujvo (l), grammar (gr), correct (c), more (m), help (h, commands),
info (about)
```

The words in brackets are short-hands/aliases. Easier to type when you're rushing about trying to make a response to someone!

## Lookup

Look up entries according to the word itself:

```
<chrisdone> h v
<lojbot> valsi (v): lookup a gismu/cmavo/lujvo/fu'ivla

<chrisdone> v valsi
<lojbot> gismu {valsi}, with rafsi {val, vla}, glossing to "word": x1
is a word meaning/causing x2 in language x3; (adjective: x1 is
lexical/verbal).
```

Words don't have spaces in, so we can reasonably assume when you enter more than one word, you want to see more than one result:

```
<chrisdone> v ganra garna grana
<lojbot> gismu {ganra}, with rafsi {gan}, glossing to "broad": x1 is
broad/wide in dimension x2 [2nd most significant dimension] by
standard x3.
<lojbot> gismu {garna}, with rafsi {gar}, glossing to "rail", "bar",
"railing": x1 is a rail/railing/bar [tool] supporting/restraining x2,
of material x3.
<lojbot> gismu {grana}, with rafsi {ga'a}, glossing to "stick",
"pole", "staff", "cane", "rod": x1 is a rod/pole/staff/stick/cane
[shape/form] of material x2.
```

If we know the start of a gismu, or something like it, we can use a wild card to search for it. Here's an example of words starting with {jd}, a cluster which many find difficult to pronounce:

```
<chrisdone> v jd*
<lojbot> gismu {jdari}, with rafsi {jar}, glossing to "firm": x1 is
firm/hard/resistant/unyielding to force x2 under conditions x3. .. 11
more results
<chrisdone> more
<lojbot> gismu {jdice}, with rafsi {jdi}, glossing to "decide": x1
(person) decides/makes decision x2 (du'u) about matter x3
(event/state). .. 10 more results
<chrisdone> more
<lojbot> gismu {jdika}, glossing to "decrease": x1 (experiencer)
decreases/contracts/is reduced/diminished in property/quantity x2 by
amount x3. .. 9 more results

<chrisdone> v xa*m*
<lojbot> cmavo cluster {xamoi}, of selma'o MOI*, glossing to "is
sixth among": quantified selbri: convert 6 to ordinal selbri; x1 is
sixth among x2 ordered by rule x3. .. 15 more results
<chrisdone> more
<lojbot> gismu {xajmi}, with rafsi {xam}, glossing to "funny": x1 is
funny/comical to x2 in property/aspect x3 (nu/ka); x3 is what is funny
about x1 to x2. .. 14 more results
```

Next we have the 'rafsi' command. Search gismu/cmavo by their rafsi:

```
<chrisdone> h r
<lojbot> rafsi (r): find gismu/cmavo with the given rafsi

<chrisdone> r loj ban
<lojbot> gismu {logji}, with rafsi {loj}, glossing to "logic": x1
[rules/methods] is a logic for deducing/concluding/inferring/reasoning
to/about x2 (du'u).
<lojbot> gismu {bangu}, with rafsi {ban, bau}, glossing to
"language": x1 is a/the language/dialect used by x2 to
express/communicate x3 (si'o/du'u, not quote).

<chrisdone> r sel
<lojbot> cmavo {se}, of selma'o SE, with rafsi {sel}, glossing to
"2nd conversion": 2nd conversion; switch 1st/2nd places.
```

Similar to the valsi command, it can display more than one result at a time because it is clear that you want this. Wild cards also work with rafsi, but I have yet to see why this is useful. I will leave it in, though.

Gloss are a very helpful way to search lojban vocabulary:

```
<chrisdone> h g
<lojbot> gloss (g): find valsi with the given gloss

<chrisdone> g cat
<lojbot> gismu {mlatu}, with rafsi {lat}, glossing to "cat": x1 is a
cat/[puss/pussy/kitten] [feline animal] of species/breed x2;
(adjective:) x1 is feline. .. 32 more results

<chrisdone> g speaking
<lojbot> cmavo {sa'e}, of selma'o UI3, glossing to "precisely
speaking": discursive: precisely speaking - loosely speaking. .. 2
more results

<chrisdone> g love
<lojbot> cmavo {iu}, of selma'o UI1, glossing to "love": attitudinal:
love - no love lost - hatred. .. 8 more results
<chrisdone> more
<lojbot> lujvo {pampe'o}, with rafsi {pam, pe'o}, selrafsi {prami,
pendo} (friend), glossing to "boyfriend", "lover", "girlfriend":
pr1=pe1 is a lover of pr2=pe2. .. 7 more results
<chrisdone> more
<lojbot> lujvo {pamsrasu}, with rafsi {pam, srasu}, selrafsi {prami,
srasu} (grass), glossing to "lovegrass": x1 is lovegrass of
species/variety x2. .. 6 more results
```

I have demonstrated the more command a few times. It simply displays the next result. It used to display three or four results at a time. But it's usually the next result you want. And if you're listing more than a few results, you should use a proper mass lookup tool. Also, lojbot waits a few seconds every three messages to stop it flooding the IRC server. Keep that in mind.

Indeed, wildcards work with the gloss, too:

```
<chrisdone> g *plant*
<lojbot> fu'ivla {akmela}, glossing to "toothache plant",
"paracress", "spotflower": x1 is a toothache
plant/spotflower/paracress of species/variety x2. .. 7 more results
<chrisdone> more
<lojbot> lujvo {derba'o}, with rafsi {der, ba'o}, selrafsi {dertu,
banro} (dirt, grow), glossing to "to sprout [of plant]", "sprouting
object", "sprout": b1 initially grows b2 beyond the soil; b1 sprouts
b2 from the ground. .. 6 more results
```

The definition search is nice. It will try to give you the best matching result first. A good match is an exact word:

```
<chrisdone> h d
<lojbot> definition (d): search for valsi(s) by definition

<chrisdone> d dances
<lojbot> gismu {dansu}, glossing to "dance": x1 (individual, mass)
dances to accompaniment/music/rhythm x2.
```

Above we see that we have matched a single word exactly. And now, we match a word inside the slashes:

```
<chrisdone> d music
<lojbot> gismu {dansu}, glossing to "dance": x1 (individual, mass)
dances to accompaniment/music/rhythm x2. .. 6 more results
```

No problem.

Indeed, this command will try the separate words, too, to see what comes up better, which is useful for when you want to search a few ideas:

```
<chrisdone> d teacher teaches
<lojbot> gismu {ctuca}, with rafsi {ctu}, glossing to "teach": x1
teaches audience x2 ideas/methods/lore x3 (du'u) about subject(s) x4
by method x5 (event).
```

But what about incomplete words?

Let's say I want all results relevant to teaching, I can use a wild card:

```
<chrisdone> d *teach*
<lojbot> gismu {ckule}, with rafsi {kul, cu'e}, glossing to "school":
x1 is school/institute/academy at x2 teaching subject(s) x3 to
audien./commun. x4 operated by x5. .. 3 more results
<chrisdone> more
<lojbot> gismu {ctuca}, with rafsi {ctu}, glossing to "teach": x1
teaches audience x2 ideas/methods/lore x3 (du'u) about subject(s) x4
by method x5 (event). .. 2 more results
<chrisdone> more
<lojbot> lujvo {balcu'e}, with rafsi {bal, cu'e}, selrafsi {banli,
ckule} (great, school), glossing to "university", "university site",
"course", "student", "regents": c1 is a university at c2 teaching
subject(s) c3 to audience/community c4 operated by c5. .. 1 more
result
<chrisdone> more
<lojbot> lujvo {ctununta'a}, with rafsi {ctu, nun, ta'a}, selrafsi
{ctuca, nu, tavla} (teach, event abstract, talk), glossing to
"lecture", "lecturer": n1=c5 is a lecture / an event of verbal
teaching by t1=c1 to audience $t_2 = c_2$ about subject $t_3 = c_4$ in
language t4 with facts taught c3.
```

A nice way to investigate cmavo of a particular selma'o, especially after just viewing a cmavo entry and wondering what else there is in that selma'o, is to use the s (for selma'o) command:

```
<chrisdone> h s
<lojbot> selma'o (s): list cmavo of a selma'o

<chrisdone> s UI5
<lojbot> {be'u}: lack, {dai}: empathy, {fu'i}: easy, {ga'i}: hauteur,
{ju'o}: certainty, {le'o}: aggressive, {ri'e}: release of emotion,
{se'a}: self-sufficiency, {se'i}: self-oriented, {vu'e}: virtue,
{zo'o}: humorously .. 11 more results
<chrisdone> more
<lojbot> cmavo {be'u}, of selma'o UI5, glossing to "lack":
attitudinal modifier: lack/need - presence/satisfaction -
satiation. .. 10 more results
<chrisdone> more
<lojbot> cmavo {dai}, of selma'o UI5, glossing to "empathy":
attitudinal modifier: marks empathetic use of preceding attitudinal;
shows another's feelings. .. 9 more results
```

The translation command uses jbofihe:

```
<chrisdone> h t
<lojbot> translate (t): translate some lojban with jbofihe -x

<chrisdone> t coi ro do
<lojbot> (^coi /greetings/ ro /every/ /(of)/ do /you/^)
<chrisdone> t mi ui nelci la djan
<lojbot> [([nelci1 (like-r(s)):] mi /I, me/ .ui /{happiness..}/)
/[is, does]/ <<nelci /lik-ing/>> ([nelci2 (liked thing(s)):] la / /
djan. /[NAME]/)]
<chrisdone> t mi nelci la djan iu ru'e
<lojbot> [([nelci1 (like-r(s)):] mi /I, me/) /[is, does]/ <<nelci
/lik-ing/>> ([nelci2 (liked thing(s)):] la / / djan. /[NAME]/ .iu ru'e
/{weak love..}/)]
```

Unfortunately it doesn't yet give informative error messages:

```
<chrisdone> t do mo do mo
<lojbot> parse error
```

But I will do that some time.

The selrafsi command gives you the lujvo which the given selrafsi is in:

```
<chrisdone> h sr
<lojbot> selrafsi (sr): find all lujvo with given selrafsi

<chrisdone> sr gleki
<lojbot> bebgei (giddy), gekpanpi (blissful), tcegei (overjoyed) .. 3
more results
<chrisdone> more
<lojbot> lujvo {bebgei}, with rafsi {beb, gei}, selrafsi {bebna,
gleki} (foolish, happy), glossing to "giddy": g1 is giddy about
g2. .. 2 more results
<chrisdone> more
<lojbot> lujvo {gekpanpi}, with rafsi {gek, panpi}, selrafsi {gleki,
panpi} (happy, peace), glossing to "blissful": g1=p1 is blissful about
g2=p2 (event/state). .. 1 more result
<chrisdone> more
<lojbot> lujvo {tcegei}, with rafsi {tce, gei}, selrafsi {mutce,
gleki} (much, happy), glossing to "overjoyed", "delighted",
"ecstatic", "delightful": g1 is delighted/ecstatic/overjoyed about g2
(event/state).

<chrisdone> sr gletu
<lojbot> cpanygle (ride), datpavycinglepre (straight), glebre (heat),
glefi'a (pornography), glepe'o (sexual partner), gleve'u (prostitute),
mitcinglepre (MOTSS), mitpavycinglepre (homo), nu'ogle (virgin),
pavycinglepre (non-bisexual), relcinglepre (bisexual) .. 11 more
results
```

The cll command looks up, using Google, results from The Lojban Reference Grammar:

```
<chrisdone> h cll
<lojbot> cll: lookup something in the lojban reference grammar

<chrisdone> cll ko'a
<lojbot> http://jbotcan.org/cllc/c7/s5.html : Brevity Is The Soul Of
Language: Pro-sumti And Pro-bridi - The ...There are ten cmavo in the
ko'a-series, and they may be assigned freely to any sumti
whatsoever. The English word ``he'' can refer only to males, ... .. 9
more results
```

valid simply tells us that some grammar is valid.

```
<chrisdone> h c
<lojbot> correct (c): camxes test for validity

<lojbot> valid: coi ro do
<chrisdone> correct coi ro d
<lojbot> not valid: coi ro d
```

And finally, lujvo makes a lujvo for you:

```
<chrisdone> h l
<lojbot> lujvo (l): construct lujvos from selrafsis and rate them

<chrisdone> lujvo xamgu dansu
<lojbot> xagdansu (7937), xaurdansu (9006)
```

Ah, I forgot the search command. It is a combination of the other look up commands, showing results of the highest priority first. It goes:

```
valsi > rafsi > gloss > definition
```

Thus, {bau} matches a rafsi, which means no valsi entries matched:

```
<chrisdone> q bau
<lojbot> cmavo {bau}, of selma'o BAI, glossing to "in language":
bangu modal, 1st place in language ... .. 1 more result
{bridi} matches a valsi, and 62 more results, meaning other things like gloss and definition results matched against "bridi", but they are less precise, and so are listed later:

<chrisdone> q bridi
<lojbot> gismu {bridi}, with rafsi {bri}, glossing to "predicate": x1
(text) is a predicate relationship with relation x2 among arguments
(sequence/set) x3. .. 62 more results
<chrisdone> more
<lojbot> lujvo {brirebla}, with rafsi {bri, rebla}, selrafsi {bridi,
rebla} (predicate, tail), glossing to "bridi-tail": x1 is a bridi-tail
of predicate relationship x2 with relation x3 among arguments
(sequence/set) x4 . .. 61 more results
```

Here we have a gloss match, meaning no valsi or rafsi entries matched.

```
<chrisdone> q precisely speaking
<lojbot> cmavo {sa'e}, of selma'o UI3, glossing to "precisely
speaking": discursive: precisely speaking - loosely speaking. .. 4
more results
```

And here a definition match, because "dances" doesn't match any of the valsi, rafsi or gloss entries.

```
<chrisdone> q dances
<lojbot> gismu {dansu}, glossing to "dance": x1 (individual, mass)
dances to accompaniment/music/rhythm x2.
```

I might recommend this to newbies, or people who prefer to stick to one mildly clever command than use different ones. But you get more power using the individual ones; so I'd only recommend this for valsi/rafsi/gloss lookup of singular words.

## Mlismu

Lojbot has a mlismu[^3] feature, invoked by COI or DOI cmavo[^4], which makes grammatically and ontologically valid utterances, which may or may not have relevance to what was said to lojbot. Mlismu generates these utterances using fatci (facts), which are very simple statements such as that “all humans are persons”.

```
<chrisdone> coi lojbot pei
<lojbot> ckule lo va'i te nicte .o'e be le se murse be'o fo mi le
prenu
```

Some amusing instances of lojbot's bantering:

Discussing bananas and religion:

```
19:46 < chrisdone> ma grute doi lojbot
19:46 < lojbot> re'e lo se lebna be mi'o bei ko be'o .u'a fo'o cu grute
19:46 < chrisdone> zo re'e u'i
19:48 < timonator> lijda la lojbot lo nu se lebna fi lo badna
```

Roughly translated as:

```
19:46 < chrisdone> What is a fruit, O, Lojbot?
19:46 < lojbot> [Religious feeling] YOU, be the one whose fruit we
sieze! [Gain]
19:46 < chrisdone> ‘Religious’, haha.
19:48 < timonator> Lojbot believes in a religion with practices of
taking bananas from people.
```

(Go to the xamselsku page)

```
19:32 < timonator> doi lojbot gletu pei
19:32 < lojbot> gletu fa ko le danlu
19:32 < timonator> oi
19:33 <@Broca> doi lojbot do mutce tolclite
19:33 < lojbot> lo penmi va'i cu mrobi'o
```

Rough translation:

```
19:32 < timonator> O, Lojbot, fucking; how do you feel about it?
19:32 < lojbot> Go fuck an animal.
19:32 < timonator> Hey! >:-(
19:33 <@Broca> O, Lojbot, you are very rude.
19:33 < lojbot> The encounterer, in other words, dies.
```

(Go to the xamselsku page)

It's surprisingly coherent, sometimes.

## Source

Lojbot is written in Haskell; you can view its complete source code or pull it with git:

```
git clone git://github.com/chrisdone/lojbot.git
```

Okay, so I said I wouldn't discuss Haskell, but I wrote a neat wild card library which Lojbot uses for the above described wild card stuff. It's pretty concise, I think. The pattern matching makes it easy to analyse the state of the function. I'm pretty sure it's essentially a state machine. I've never written a wild card library. A regular expression library would be cool to write some time. I believe there are exceedingly fast algorithms for doing wild cards which I have not used in this simple code. Maybe I'll upgrade it some time and blog about it.

## Bugs / feedback

Please add any ideas/bugs/feedback you want to the lojbot lighthouse page. That way, I will actually get them done if they're recorded somewhere.

Please join the #lojbot channel to discuss and test the bot.

[^1]: We typically use { and } to quote Lojban text, in English.

[^2]: “An IRC bot is a set of scripts or an independent program that connects to Internet Relay Chat as a client, and so appears to other IRC users as another user. It differs from a regular client in that instead of providing interactive access to IRC for a human user, it performs automated functions.” — IRC bot at Wikipedia

[^3]: Mlismu is a program written by Adam Lopresto which “[Generates] semantically near-valid grammatical lojban nonsense.” — mlismu's web page

[^4]: That is, words which are vocatives, such as {doi djan}, {ki'e djan}, {je'e djan}, etc. for "O, John", "Thanks, John", "Roger, John", etc.
