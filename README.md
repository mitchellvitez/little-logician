# Little Logician

&ldquo;Logical sequences follow in due course when we have arranged
the propositions thus.&rdquo;

&ndash; Aristotle, _Organon_

## What it is

Little Logician (henceforth, LL) is a tiny proof system for classical propositional logic. For example, say I want to prove that (not b) follows from (b). Here's that "proof" in LL:

```
b ⊢ ¬b
```

Obviously, we can't derive the opposite of a proposition from the proposition. So our goal to prove that b leads to not b can't be proven by LL. We say the goal "does not obtain".

Here's a slightly more complicated example. We're now using the logical connective ∧ ("and"), and we have multiple premises on the left side. Note that we can use connectives on the right side as well: any valid proposition is allowed on the right side, but (unlike on the left) we can't have multiple of them.

```
a ∧ b, b ∧ ¬c ⊢ a ∧ ¬c
```

We could read the above proof as "a and b, (along with) b and not c, leads to a and not c".  In fact, LL's notation is flexible enough that we can pretty much just write that down:

```
a and b, b and not c leads to a and not c
```

To prove multiple goals at once, write them down separated by semicolons in some file, and run `stack run filename` on that file. Here's some sample output showing some more proofs:

```
a, a → ¬b ⊢ b                      does not obtain
a, a → b ⊢ b                       obtains
a, a → b ⊢ a ∨ b                   obtains
b ⊢ ¬b                             does not obtain
a ∧ b ∧ ¬a ∨ b ⊢ a                 obtains
¬b ↔ a ∧ b ⊢ a                     does not obtain
a ∧ b, b ∧ ¬c ⊢ a ∨ c              obtains
a ∧ b, b ∧ ¬c ⊢ a ∧ ¬c             obtains
a ∧ b, b ∧ ¬c ⊢ a ∧ c              does not obtain
a → b → a ⊢ a                      does not obtain
a → b ∧ b → c, a ⊢ c               does not obtain
a ↔ ¬a ⊢ F                         obtains
a ↔ ¬a ⊢ b                         obtains
A and B and (not A or B) proves A  obtains
A and B iff not B leads to A       does not obtain
8 out of 15 goals obtained
```

## How it works

First, a fancy Megaparsec parser turns text into a small internal logical language. Then, using that language, we start performing a proof by contradiction.

First, we unify our proof system by negating the consequent, and moving it to the left hand side. This means our system only has to prove that a list of propositions contains a contradiction within it.

```
a, a → b ⊢ b
a, a → b, ¬b ⊢ False 
```

Now we just have a list of propositions. We perform a first pass (which I called "normalization" in the code) to reduce all propositions to some combination of And, Or, Not, and variables. This means rewriting things like implications and biconditionals. It also means we can extend the proof system later (with connectives like XOR), by defining new connectives in terms of And, Or, and Not.

```
[a, a → b, ¬b]
[a, ¬a ∨ b, ¬b]
```

Now that we've rewritten complex connectives, we can perform a reduction on our collection of Ands, Ors, Nots, and variables. For example, a ∧ b is the same as having both a and b in our list of propositions (`[a, b]`). a ∨ b means that we can split into two cases: a, and b, and prove either case.

```
[a, ¬a ∨ b, ¬b]
[a, ¬a, ¬b] [a, b, ¬b]
```

There are two lists now, because we split the or into two cases. Next, we can look for contradictions. If we see any propositions of the form p and not p in the same list, we have a contradiction.
We can output that this goal obtains:

```
a, a → b ⊢ b  obtains
```

## Notes on Notation

Variable names must start with a letter, but can then contain any combination of letters, digits, `_`, and `'`.

There are many options for how to notate each logical connective, so you can pick what you like, ranging from mathy to easy-to-type to englishy. You can even mix and match. LL will respect your decisions and output your proofs exactly as you entered them.

The numbers in parentheses are precedence levels: higher number means higher precedence.

- negation (3)<br>
  `¬`, `~`, `not`
- truth<br>
  `T`, `True`, `true`
- falsehood<br>
  `F`, `False`, `false`
- and (2)<br>
  `∧`, `&`, `and`
- or (1)<br>
  `∨`, `|`, `or`
- implication (0)<br>
  `→`, `→`, `->`, `implies`
- biconditional<br>
  `↔`, `⇔`, `<->`, `iff`
- turnstile<br>
  `⊢`, `|-`, `proves`, `leads to`
