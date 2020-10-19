# H2SPEC
Compute the spectral line list and the vacuum ultraviolet spectrum of H2

<h3>Description</h3>
<p>The program h2spec computes a list of allowed optical transitions for
the Lyman (*B-->X*) and Werner (*C-->X*) bands of the hydrogen molecule,
using avaialable experimental and theoretical data. The generated line
list is written to a file. h2spec also uses the line list to compute
the vacuum ultraviolet spectrum of the H<sub>2</sub> molecule, under
specified excitation conditions and spectral resolution.</p>

<h3>Build</h3>
<p>Under Linux, gfortran may be used to build h2spec from source:<br><br>

	$ gfortran -o h2spec h2spec.f
<br>
<br>

<h3>References</h3>
<p>
1. K. Myneni and J. Kielkopf, *Excited-state populations of* H2 *in the positive column of a glow discharge*, J. Phys. B **21**, 2871--2878 (1988).
2. J. Kielkopf, K. Myneni, and F. Tomkins, *Unusual fluorescence from* H2 *excited by multiphoton processes*, J. Phys. B **23**, 251--261 (1990).
</p>

