+++
title = "Some Light in the Background"
date = "2020-04-15T09:22:34+05:30"
slug = "light"
+++


#### tl;dr
> Dark backgrounds are the best. But when one accidentally land
> somewhere with a light background with eyes trained for dark, it can
> hurt. So, one may as well just make everything light for
> consistency.


We love dark backgrounds. They are easier on the eyes, comfortable to
work with in the night/dark rooms. So, we'd like everything that
we see on the screen to be dark. And that's the key, _everything_ must
be dark.

Imagine working on the computer with a completely dark environment.
Every app configured to be at its darkest. You find the need to look
something up on the web. No worries, the search engine supports dark
mode, so that's good. Hey, the 2nd search result _is_ what you were
looking for and you open it. Then, an extreme bright light hits you
hard. The article is built with a white background, in a way that all
your hacks to make (_most_) websites dark doesn't work. The eyes hurt,
the head hurts a bit.

I have faced this, multiple times. As someone who is extremely
sensitive to brightness, sudden change is extra annoying. So, a few
years ago, I had to switch everything to be light. With that, such
events never happen.

I hope at some point dark backgrounds become the default everywhere.
Or, users should be able to tell the browsers their dark/light mode
preference, which can then be forwarded to websites. With that, I'd
happily switch back to making everything dark.

### Update
Now, this website supports dark mode based on a user's system
preference. It uses [prefers-color-scheme](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme)
media query to achieve that.

```css
@media (prefers-color-scheme: dark) {
  /*
   * Custom CSS code / overrides go here for dark mode go here.
   * They which will be applicable for users who have set `dark` as a
   * system color scheme.
   */
}
```
