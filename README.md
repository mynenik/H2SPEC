# H2SPEC
Compute the spectral line list and spectrum of H<sub>2</sub> in the
vacuum ultraviolet region.

<h3>Description</h3>
<p>The program <tt>h2spec</tt> computes a list of allowed optical
transitions for the Lyman (<i>B&rarr;X</i>) and Werner (<i>C&rarr;X</i>)
bands of the hydrogen molecule, using available experimental and 
theoretical data. The generated line list is written to the file,
<tt>h2lines.dat</tt>. <tt>h2spec</tt> also uses the line 
list to compute the vacuum ultraviolet spectrum of the H<sub>2</sub> 
molecule, under specified excitation conditions and spectral 
resolution. The spectrum is written to the file, <tt>h2vuv.dat</tt>.
</p>

<p><tt>h2spec</tt> was used in the research reported in References
1--4.</p>

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
3. J. F. Kielkopf and N. F. Allard, <i>Satellites on Lyman alpha
due to</i> H-H <i>and</i> H-H<sup>-</sup> <i>collisions</i>,
Astrophysical Journal <b>450</b>:L75--L78 (1995).<br>
4. J. F. Kielkopf and N. F. Allard, <i>Observation of the far wing
of Lyman &alpha; due to neutral atom and ion collisions in a
laser-produced plasma</i>, Phys. Rev. A <b>58</b>, 4416--4425
(1998).<br>

