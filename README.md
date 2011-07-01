# housetab: A webapp for sharing expenses.

## Using

Anyone is free to install this on their own server, do anything with any of the code, etc. The only restrictions are on the design - it is provided with a creative commons license that says you cannot use it commercially. We put a lot of time into it, and feel pretty strongly that this should be a free service for people, so we don't want anyone to be able to turn around and start charging for it (granted, being written in Haskell, I'm not sure if that would ever happen, but). But if you want to also give it away for free, clone this whole repository and go wild! Or, you can take any of the code, come up with your own design, and do whatever you want with it. 

## Install

This should be cabal installable, once you install some extra libraries that are not on hackage (at least at the time of this writing). They are:

    https://github.com/snapframework/snap-auth/
    https://github.com/ozataman/snap-extension-mongodb
    https://github.com/dbp/heist-async
    https://github.com/dbp/digestive-functors-snap-heist
    https://github.com/dbp/email-postmark
    https://github.com/dbp/snap-logging-mongodb

Beyond that, the only other thing you need running is MongoDB. It is developed with 1.8, but we are not doing anything particularly fancy, so I would assume it would work with earlier versions as well. We assume it is cunning on the default port. The generated binary is "housetab" and takes all the regular commandline options that snap servers do. 

## Roadmap

### API
I would really like to build in some API support to allow people to bring data with them and take it if they decide to go. In an earlier version of this app, this functionality was presented in a textarea on the site, but the fact that if someone hit "load data" and had the field empty, it would wipe out all their info, I decided that was a little too dangerous. Some possible API endpoints would be:

    /export/all -- would give all entries, info on people, etc, in machine readable format (probably haskell Show for lists/tuples)
    /export/STARTDATE/ENDDATE -- would just give entries / shares for people for a time range
    /import -- would take in entries / shares and add them into the account
    /load -- would wipe out everything in the current account and replace it with what is uploaded

### Per category shares
The system now supports (well, enforces) entering entries in by category, but all it is is a little more metadata - it doesn't actually allow you to do anything differently. The intent was to allow people to put in shares by category - so they would have an overall share, but then they could override it per category, but that hasn't been built yet

### Multiple currencies
It was pointed out in the development of this that there is no reason why this has to deal with money at all - anything that serves as a currency could use it, with time being an obvious example. People can enter in work they do for a group or for each other, and the balance of labor is recorded. That of course works right now, so long as you make an account that only deals in time, and ignore the dollar signs. What would be neat is to have a way to have multiple currencies on the same account at the same time. Then the question becomes whether there should be two totally different balances (so the two currencies exist in one account just to make it more convenient than having two accounts, but doesn't add any functionality), or whether there should be some equivalency, ie, labor is worth $15/hr, so if I pay for less things, but do a lot more collective work, we balance out. That's an open question, but it is a pretty interesting idea, and brings up some really interesting ideas of where HouseTab could go... 