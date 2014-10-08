# Libertree-flavoured markdown

You can style text on Libertree with Markdown syntax.  The following
sections describe the extended syntax Libertree supports.


## Preformatted blocks

<div class="example"><pre>
This is an ASCII GNU by Martin Dickopp

~~~
  ,= ,-_-. =.
 ((_/)o o(\_))
  `-'(. .)`-'
      \_/
~~~
</pre></div>

This will render like this:

This is an ASCII GNU by Martin Dickopp

~~~
  ,= ,-_-. =.
 ((_/)o o(\_))
  `-'(. .)`-'
      \_/
~~~


## Hidden blocks

<div class="example"><pre>
I watched a movie.  It was great!

?> Especially when the bear appeared out of nowhere
?> and ate all the honey.
</pre></div>


I watched a movie.  It was great!

?> Especially when the bear appeared out of nowhere
?> and ate all the honey.


## HTML5 Videos

<div class="example"><pre>
%[Short description](http://example.com/video.ogv)
</pre></div>

%[Richard Stallman's talk at TEDx](http://audio-video.gnu.org/video/TEDxGE2014_Stallman05_HQ.ogg)


## HTML5 Audio

<div class="example"><pre>
~[Short description](http://example.com/music.ogg)
</pre></div>


~[Short description](http://radioserver1.delfa.net:80/64.opus)


## Footnotes

<div class="example"><pre>
This is a sentence with a footnote[^1].
This is a another sentence.

[^1]: This is the footnote.
</pre></div>


This is a sentence with a footnote[^1].
This is a another sentence.

[^1]: This is the footnote.
