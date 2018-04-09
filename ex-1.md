# Exercise 1

We want to preview for the user the size of the file they will be building.

Our goal is an HTML \<p> tag with the content "Projected file size \<N>mb."

We want this to dynamically update as the user edits the email volume input field, disappearing altogether if the field is blank.

Some help:
* File sizes are roughly one megabyte per ten thousand emails.
* The \<p> tag is already in place ready for you to replace "Your solutuion here" with a CLJS form returning "Projected file size \<N>mb.".
* The Matrix incarnation of the DOM volume \<input> is returned by `(fmo "email-volume")`. Trust us on this at this early stage.
* The current value of the DOM volume widget can be accessed as (<mget V :value), where V is the Matrix incarnation of the \<input>. Note that the value will be the string value as offered by the DOM element.
Some things you will get for free:
* We track `oninput` instead of `onchange` to satisfy "as the user edits". 
* The `mset!>` setter initiates propagation of any change to any dependents, so that id done.
* The `<mget` getter sets up the dependency that gets its containing formula re-executed as needed. 
* A so-called "observer" provided by the mxWeb framework will relay the string to the DOM as it changes.

