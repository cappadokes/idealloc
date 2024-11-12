1. My documentation states that a `JobSet` is *always* sorted by increasing birth. Does this hold?

2. The input is, of course, a set of Jobs.

The output is:

i) 	a 1-1 mapping of each Job to a proper offset
ii)	the corresponding makespan (i.e., size memory that needs to be allocated in total)

The following info complicate things:

- idealloc is probabilistic: multiple iterations yield different makespans
- competition such as minimalloc accept an extra parameter denoting the "maximum allowable makespan" in absolute terms.

With regard to the second point, we *could* use it to decide if the makespan produced fits (we interpret the parameter in question as the size of available physical, contiguous memory). But it is bad decision: if the value is smaller than the Jobs' max load, the request is impossible to fulfill. If it's too big, we are facing the risk of otherwise avoidable fragmentation.

In our opinion the healthiest way to proceed is by replacing the makespan parameter with a WORST-CASE FRAGMENTATION parameter, which is naturally a function of the max load. If after "too many" iterations take place and we're still above the limit, idealloc should return an error (nevertheless containing the best achieved placement).

What is "too many", though?

- either a hardcoded number
- either as many iterations as fit in a pre-defined time limit (computed by self-measurement)
- either a result of statistical inference, conducted offline (because measuring all makespans online instead of early-exitting best-fit would harm latency)

3. 