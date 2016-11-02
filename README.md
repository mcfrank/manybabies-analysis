# ManyBabies Analysis

Analysis plan for the [ManyBabies 1 study](https://osf.io/re95x/). Contains both initial simulations and then later analysis of pilot data, fixing analytic decisions for the confirmatory analyses. 

Paper draft can be found [here](https://docs.google.com/document/d/1kv3zZ2ylKHlfu779Xw8DaKxUEBHAa56B-4sv-GRvuBU/edit).

# TODOs
- Implement further analyses so that the pilot analysis has the structure of the real analysis
- Design appropriate templates for labs to use for data entry
- Discuss decisions about plotting and analysis conventions

# Discussions pasted from emails
So it sounds like LT, rather than total trial length, is the consensus from the human coder side :)  

In terms of providing data templates/information requested, I think we should put total trial length down as a requested data anyway.  'Total LT' seems to be (fortunately) defined in a pretty standard way but it's a summary measure that collapses a richer kind of data: looks on/off every 0.1 second/whatever -> sum(looks on until a look off for n seconds)   

I guess it matters if we want to be able to just document whether distributional differences in LT measures occur across methods, or give a finer-grained account of why these differences happen (e.g. because eyetracking trials are generally longer with shorter looks while headturn measures shorter, focused attention, or whatever).  

Hugh, I think your point about 2-second LTs is going to be a problem in either case: a trial could be 4s long but have only 1s of LT total (1s inattention, 1s look, 2s lookaway), 4s long with 2s LT, 2s long and 2s of LT (Right? from track loss), 2.1s long and .1s of LT ('real' data with a very short look), etc.  

Mike - I'm having trouble running the code right now (I just need to learn to use rmd format, I'll figure it out) so can't see the grouped dataframe: does the track loss condition just mean scenarios where the tracker never picks up a look at all?

What's the standard for human coding if you never pick up an initial look?

I think this could work across methods: All trials should be a minimum of 2s because that's the lookaway criteria (right?) Drop trials that terminate before 2s (for whatever reason). Drop trials where you never got an initial look (human coded, or extracted from eyetracking data), otherwise include.  To do this, we'd need everyone reporting at least LT and total trial length, but nothing more fine grained (I would love to give people the *option* to give us timecourse data if they have it). 

1) Why does Mike's setup produce so many 2s responses?

I assume that this is because of track loss trials? But I like your suggestion to only include trials with LT >2s, assuming that is the look-away criterion for all setups (tho thinking outloud, how would this work with eg single screen, for which there will be some trials on which the child looks away instantly, but where the LT >2 because of the reaction time of the coder).

>>I was referring to just the eyetracking data actually, since I think I have a mental model that says that headturn, single screen, and eyetracker # of seconds LT are not actually directly comparable anyway.   If nothing else, we have timecourse data on the eyetracker, whereas it looks like the other 2 datasets we are just getting the total summed time (right?)  However actually adjusting LTs by 2 seconds (I was proposing subtracting 2 sec in addition to dropping shorter trials) might stretch this beyond reasonableness; slash maybe I should be more sanguine about the # of seconds converging across methods.  Mike? 


2) We shouldn't do between subjects test.

Yes, I don't see a compelling motivation for this either.

>>You mean between conditions?  Anyway, if we proceed with #3 below it would seem like we should include IFF it's a common analysis.  

3) Is the goal of the analyses to (1) present what we believe to be the most appropriate analysis? (2) To reflect current practices? (3) to compare the two or (4) something else?

I would go with (1), but am happy to be corrected. It would also be helpful to do (3).


4) Why not mixed effects structures (esp. for Subjects and Items) in other models, too?
You're right that I should have included random Subjects effects in these models. They weren't in the original planned analysis presumably because we were thinking of using difference scores, which would be calculated over all trials, and so would remove all that structure from the data.
ItemIdentity is not in the datafiles right now.

5) cant we measure item effects on preference scores too, since the items are paired across condition?
Yes we could do this, but wouldn't it have the (bad) effect that if we have an NA for one of the trials, then we have to exclude the data from its paired  trial as well? That seems wasteful. 

>>Not sure if we're talking about the same thing: If we are doing by-(mega)trial differences in LT for each item, we'd have to drop pairs where one of the LTs can't be calculated, right?


