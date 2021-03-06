+++
title = "You have infinite email addresses"
slug = "infi-mail"
date = "2016-04-02T00:00:00Z"
publishdate = "2016-04-02T00:00:00Z"
+++

> **tl;dr**  
> You can add arbitrary tags to your email address. Any email sent to
> `your-email+tag@your-provider.ext` will land in the inbox for
> `your-email@your-provider.ext`.

Emails are important. They are your identity on the web. Just like on phone,
you'd want to control who can contact you via email. This warrants caution in
signing up at places on the internet. It can be unsafe to register with the
personal email address on new services for multiple reasons:

 - Bulk data breaches [are fairly common][data-breaches]. You don't want your
   email address out there for bots around the world to feed on.
 - The service you are signing up for may itself be malicious. Or worse, it may
   be Facebook!
 - Same email address on every service can make it simpler for bots to break
   into your accounts once they succeed in attacking one.
 - There [are actual services][full-contact] dedicated for creating single
   points of catastrophic failures utilizing email addresses.

These sound upsetting, but are indeed manageable to an extent. One trick that I
use is *tagging the email address*. It is a fairly common practice among
programmers. Most popular email services support this.

What do I mean by tagging? Lets take Jon's email: `jon.snow@email.wf`. His
sister, Arya, will know this address as is. But for the untrustworthy
Lannisters, he tags the email: `jon.snow+lannister@email.wf`. When someone
writes to this address: he will receive the mails knowing that Lannisters were
involved.

> `email.wf` here is a fictional service, analogous to `gmail.com`. I didn't
> want to accidentally use an actual email address.

One may use a similar technique while signing up for services on the
internet. Say Jon wants to register on `nightswatch.got`.

 - In *Night's Watch* registration form, he'd use
   `jon.snow+nighstwatch@email.wf` as the email. The tag `+nighstwatch` here is
   arbitrarily chosen.
 - Jon will get the confirmation mail in the original account. Being prudent,
   he'll verify that the mail it does have the correct tag in the *to address*.
 - Also, `email.wf` lets Jon send emails with the tagged address. This is useful
   for talking to the customer care. GMail users can go to:  
   `Settings -> Accounts and Import -> Add another email address you own`.  
   and filling up the form with the tagged address.

This means Jon can have practically infinite email addresses. Why is this
helpful?

 - Jon can collect all the mails from *Night's Watch* at one place, specially
   when their mails aren't consistent enough to write filters.
 - If the credentials for the *Night's Watch* account were to be leaked, say
   through Phishing, other more important services like the email account itself
   would not be as vulnerable.
 - One cannot track Jon's movements across various accounts/profiles through his
   email.
 - Sites which don't allow tags in the email address can be a red flags for
   Jon. He can assume that they'd have terrible developers/management and look
   for alternates.

Making this into a habit will require a bit of discipline. But eventually, it
gets instinctive.

On a related note, [this][phishing-book] is an interesting book, that talks
about Phishing through email in the real world.

[data-breaches]: http://www.huffingtonpost.com/entry/biggest-worst-data-breaches-hacks_us_55d4b5a5e4b07addcb44fd9e
[full-contact]: https://www.fullcontact.com/gmail/
[phishing-book]: http://www.amazon.com/gp/product/1118958470/ref=as_li_qf_sp_asin_il_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1118958470&linkCode=as2&tag=crodjer-20&linkId=244BG6VSA5AT2K5S
