# Revision history for data-elevator-forced

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2023-04-14

* Use a StrictValueExtractor instead of a raw Strict.

This avoid a common pitfall where let bound destructuring will fallback to
lazy semantics. This way we force the use to bound the two values with
names. Any inconsistency will be reported by
-Werror=unbanged-strict-patterns .

* Use a strict tuple type called Pairy

We have more oportunities to trigger evaluation like that.

* Give a good tutorial

I am pretty proud of it.

