# H2SPEC
Compute the spectral line list and the vacuum ultraviolet spectrum of 
H<sub>2</sub>.

<h3>Description</h3>
<p>The program <tt>h2spec</tt> computes a list of allowed optical
transitions for the Lyman (<i>B-->X</i>) and Werner (<i>C-->X</i>)
bands of the hydrogen molecule, using available experimental and 
theoretical data. The generated line list is written to the file,
<tt>h2lines.dat</tt>. <tt>h2spec</tt> also uses the line 
list to compute the vacuum ultraviolet spectrum of the H<sub>2</sub> 
molecule, under specified excitation conditions and spectral 
resolution. The spectrum is written to the file, <tt>h2vuv.dat</tt>.
</p>

<h3>Build</h3>
<p>Under Linux, gfortran may be used to build h2spec from source:<br><br>

	$ gfortran -o h2spec h2spec.f
<br>
<br>

<h3>References</h3>

1. K. Myneni and J. Kielkopf, <i>Excited-state populations of</i> 
H<sub>2</sub> <i>in the positive column of a glow discharge</i>, 
J. Phys. B <b>21</b>, 2871--2878 (1988).<br>
2. J. Kielkopf, K. Myneni, and F. Tomkins, <i>Unusual fluorescence
from</i> H<sub>2</sub> <i>excited by multiphoton processes</i>,
J. Phys. B <b>23</b>, 251--261 (1990).<br>

