<div _ngcontent-quc-c228="" id="programming-exercise-instructions-content" class="guided-tour instructions__content__markdown markdown-preview"><h3 id="infinitetrees">Infinite Trees</h3>
<p>The next restriction we are now going to relax is the limitation to finite trees. It is clear that we cannot store infinite trees in finite memory, thus, we have to define the trees in a lazy fashion, such that only the subtree that is actually required is constructed, while the rest is not. Let the type</p>
<pre class="ocaml language-ocaml"><code class="hljs ocaml language-ocaml"><span class="hljs-keyword">type</span> <span class="hljs-symbol">'a</span> ltree = <span class="hljs-type">LNode</span> <span class="hljs-keyword">of</span> <span class="hljs-symbol">'a</span> * (<span class="hljs-built_in">unit</span> -&gt; <span class="hljs-symbol">'a</span> ltree) * (<span class="hljs-built_in">unit</span> -&gt; <span class="hljs-symbol">'a</span> ltree)
</code></pre>
<p>define polymorphic lazy (infinite) binary trees. Instead of storing a left and right child directly, we keep a function to construct them if ever needed. Note, that we do no longer need an <code>Empty</code> constructor, since our trees are always infinite.</p>
<p>Implement the following functions for infinite tree construction:</p>
<ol>
<li><code>layer_tree : int -&gt; int ltree</code><br>
<span><span class="katex"><span class="katex-mathml"><math><mrow><mrow><mi mathvariant="monospace">l</mi><mi mathvariant="monospace">a</mi><mi mathvariant="monospace">y</mi><mi mathvariant="monospace">e</mi><mi mathvariant="monospace">r</mi><mi mathvariant="monospace">_</mi><mi mathvariant="monospace">t</mi><mi mathvariant="monospace">r</mi><mi mathvariant="monospace">e</mi><mi mathvariant="monospace">e</mi></mrow>&amp;MediumSpace;<mi>r</mi></mrow>\mathtt{layer\_tree}\: r</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.83333em; vertical-align: -0.22222em;" class="strut"></span><span class="mord"><span class="mord mathtt">l</span><span class="mord mathtt">a</span><span class="mord mathtt">y</span><span class="mord mathtt">e</span><span class="mord mathtt">r</span><span class="mord mathtt">_</span><span class="mord mathtt">t</span><span class="mord mathtt">r</span><span class="mord mathtt">e</span><span class="mord mathtt">e</span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span style="margin-right: 0.02778em;" class="mord mathdefault">r</span></span></span></span></span> constructs an infinite tree where all nodes of the <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>n</mi></mrow>n</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.43056em; vertical-align: 0em;" class="strut"></span><span class="mord mathdefault">n</span></span></span></span></span>th layer store the value <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>r</mi><mo>+</mo><mi>n</mi></mrow>r + n</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.66666em; vertical-align: -0.08333em;" class="strut"></span><span style="margin-right: 0.02778em;" class="mord mathdefault">r</span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mbin">+</span><span style="margin-right: 0.222222em;" class="mspace"></span></span><span class="base"><span style="height: 0.43056em; vertical-align: 0em;" class="strut"></span><span class="mord mathdefault">n</span></span></span></span></span>. We consider the root as layer <span><span class="katex"><span class="katex-mathml"><math><mrow><mn>0</mn></mrow>0</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.64444em; vertical-align: 0em;" class="strut"></span><span class="mord">0</span></span></span></span></span>, so the root stores value <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>r</mi></mrow>r</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.43056em; vertical-align: 0em;" class="strut"></span><span style="margin-right: 0.02778em;" class="mord mathdefault">r</span></span></span></span></span>.</li>
</ol>
<pre><code class="hljs"><span class="hljs-comment"># layer_tree 25;;</span>
- : int ltree = LNode (<span class="hljs-number">25</span>, &lt;<span class="hljs-function"><span class="hljs-keyword">fun</span><span class="hljs-title">&gt;</span></span>, &lt;<span class="hljs-function"><span class="hljs-keyword">fun</span><span class="hljs-title">&gt;</span></span>)
<span class="hljs-comment"># layer_tree 5;;  </span>
- : int ltree = LNode (<span class="hljs-number">5</span>, &lt;<span class="hljs-function"><span class="hljs-keyword">fun</span><span class="hljs-title">&gt;</span></span>, &lt;<span class="hljs-function"><span class="hljs-keyword">fun</span><span class="hljs-title">&gt;</span></span>)
</code></pre>
<ol>
<li><code>interval_tree : float -&gt; float -&gt; (float * float) ltree</code><br>
<span><span class="katex"><span class="katex-mathml"><math><mrow><mrow><mi mathvariant="monospace">i</mi><mi mathvariant="monospace">n</mi><mi mathvariant="monospace">t</mi><mi mathvariant="monospace">e</mi><mi mathvariant="monospace">r</mi><mi mathvariant="monospace">v</mi><mi mathvariant="monospace">a</mi><mi mathvariant="monospace">l</mi><mi mathvariant="monospace">_</mi><mi mathvariant="monospace">t</mi><mi mathvariant="monospace">r</mi><mi mathvariant="monospace">e</mi><mi mathvariant="monospace">e</mi></mrow>&amp;MediumSpace;<msub><mi>l</mi><mn>0</mn></msub>&amp;MediumSpace;<msub><mi>h</mi><mn>0</mn></msub></mrow>\mathtt{interval\_tree}\: l_0\: h_0 </math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.84444em; vertical-align: -0.15em;" class="strut"></span><span class="mord"><span class="mord mathtt">i</span><span class="mord mathtt">n</span><span class="mord mathtt">t</span><span class="mord mathtt">e</span><span class="mord mathtt">r</span><span class="mord mathtt">v</span><span class="mord mathtt">a</span><span class="mord mathtt">l</span><span class="mord mathtt">_</span><span class="mord mathtt">t</span><span class="mord mathtt">r</span><span class="mord mathtt">e</span><span class="mord mathtt">e</span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord"><span style="margin-right: 0.01968em;" class="mord mathdefault">l</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: -0.01968em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord"><span class="mord mathdefault">h</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span></span></span></span></span> constructs a tree where the left and right child of every node with interval <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><mi>l</mi><mo separator="true">,</mo><mi>h</mi><mo>)</mo></mrow>(l,h)</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mopen">(</span><span style="margin-right: 0.01968em;" class="mord mathdefault">l</span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord mathdefault">h</span><span class="mclose">)</span></span></span></span></span> store the intervals <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><mi>l</mi><mo separator="true">,</mo><mfrac><mrow><mi>l</mi><mo>+</mo><mi>h</mi></mrow><mn>2</mn></mfrac><mo>)</mo></mrow>(l, \frac{l+h}{2})</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1.22511em; vertical-align: -0.345em;" class="strut"></span><span class="mopen">(</span><span style="margin-right: 0.01968em;" class="mord mathdefault">l</span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord"><span class="mopen nulldelimiter"></span><span class="mfrac"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.880108em;" class="vlist"><span style="top: -2.655em;" class=""><span style="height: 3em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span class="mord mtight">2</span></span></span></span><span style="top: -3.23em;" class=""><span style="height: 3em;" class="pstrut"></span><span style="border-bottom-width: 0.04em;" class="frac-line"></span></span><span style="top: -3.394em;" class=""><span style="height: 3em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span style="margin-right: 0.01968em;" class="mord mathdefault mtight">l</span><span class="mbin mtight">+</span><span class="mord mathdefault mtight">h</span></span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.345em;" class="vlist"><span class=""></span></span></span></span></span><span class="mclose nulldelimiter"></span></span><span class="mclose">)</span></span></span></span></span> and <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><mfrac><mrow><mi>l</mi><mo>+</mo><mi>h</mi></mrow><mn>2</mn></mfrac><mo separator="true">,</mo><mi>h</mi><mo>)</mo></mrow>(\frac{l+h}{2}, h)</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1.22511em; vertical-align: -0.345em;" class="strut"></span><span class="mopen">(</span><span class="mord"><span class="mopen nulldelimiter"></span><span class="mfrac"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.880108em;" class="vlist"><span style="top: -2.655em;" class=""><span style="height: 3em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span class="mord mtight">2</span></span></span></span><span style="top: -3.23em;" class=""><span style="height: 3em;" class="pstrut"></span><span style="border-bottom-width: 0.04em;" class="frac-line"></span></span><span style="top: -3.394em;" class=""><span style="height: 3em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span style="margin-right: 0.01968em;" class="mord mathdefault mtight">l</span><span class="mbin mtight">+</span><span class="mord mathdefault mtight">h</span></span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.345em;" class="vlist"><span class=""></span></span></span></span></span><span class="mclose nulldelimiter"></span></span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord mathdefault">h</span><span class="mclose">)</span></span></span></span></span>, respectively. The root stores the interval <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><msub><mi>l</mi><mn>0</mn></msub><mo separator="true">,</mo><msub><mi>h</mi><mn>0</mn></msub><mo>)</mo></mrow>(l_{0}, h_{0})</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mopen">(</span><span class="mord"><span style="margin-right: 0.01968em;" class="mord mathdefault">l</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: -0.01968em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span class="mord mtight">0</span></span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord"><span class="mord mathdefault">h</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span class="mord mtight">0</span></span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mclose">)</span></span></span></span></span> passed as the function's arguments.</li>
<li><code>rational_tree : int -&gt; int -&gt; (int * int) ltree</code><br>
<span><span class="katex"><span class="katex-mathml"><math><mrow><mrow><mi mathvariant="monospace">r</mi><mi mathvariant="monospace">a</mi><mi mathvariant="monospace">t</mi><mi mathvariant="monospace">i</mi><mi mathvariant="monospace">o</mi><mi mathvariant="monospace">n</mi><mi mathvariant="monospace">a</mi><mi mathvariant="monospace">l</mi><mi mathvariant="monospace">_</mi><mi mathvariant="monospace">t</mi><mi mathvariant="monospace">r</mi><mi mathvariant="monospace">e</mi><mi mathvariant="monospace">e</mi></mrow>&amp;MediumSpace;<msub><mi>n</mi><mn>0</mn></msub>&amp;MediumSpace;<msub><mi>d</mi><mn>0</mn></msub></mrow>\mathtt{rational\_tree}\: n_0\: d_0 </math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.84444em; vertical-align: -0.15em;" class="strut"></span><span class="mord"><span class="mord mathtt">r</span><span class="mord mathtt">a</span><span class="mord mathtt">t</span><span class="mord mathtt">i</span><span class="mord mathtt">o</span><span class="mord mathtt">n</span><span class="mord mathtt">a</span><span class="mord mathtt">l</span><span class="mord mathtt">_</span><span class="mord mathtt">t</span><span class="mord mathtt">r</span><span class="mord mathtt">e</span><span class="mord mathtt">e</span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord"><span class="mord mathdefault">n</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord"><span class="mord mathdefault">d</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span></span></span></span></span> constructs a tree with root <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><msub><mi>n</mi><mn>0</mn></msub><mo separator="true">,</mo><msub><mi>d</mi><mn>0</mn></msub><mo>)</mo></mrow>(n_0,d_0)</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mopen">(</span><span class="mord"><span class="mord mathdefault">n</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord"><span class="mord mathdefault">d</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mclose">)</span></span></span></span></span> and for every node with pair <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><mi>n</mi><mo separator="true">,</mo><mi>d</mi><mo>)</mo></mrow>(n,d)</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mopen">(</span><span class="mord mathdefault">n</span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord mathdefault">d</span><span class="mclose">)</span></span></span></span></span>, the left child stores <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><mi>n</mi><mo separator="true">,</mo><mi>d</mi><mo>+</mo><mn>1</mn><mo>)</mo></mrow>(n,d+1)</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mopen">(</span><span class="mord mathdefault">n</span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord mathdefault">d</span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mbin">+</span><span style="margin-right: 0.222222em;" class="mspace"></span></span><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mord">1</span><span class="mclose">)</span></span></span></span></span> and the right child stores <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo separator="true">,</mo><mi>d</mi><mo>)</mo></mrow>(n+1,d)</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mopen">(</span><span class="mord mathdefault">n</span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mbin">+</span><span style="margin-right: 0.222222em;" class="mspace"></span></span><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mord">1</span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord mathdefault">d</span><span class="mclose">)</span></span></span></span></span>.</li>
</ol>
<p>Implement the following functions to work with infinite trees:</p>
<ol>
<li><code>top : int -&gt; 'a ltree -&gt; 'a tree</code><br>
<span><span class="katex"><span class="katex-mathml"><math><mrow><mrow><mi mathvariant="monospace">t</mi><mi mathvariant="monospace">o</mi><mi mathvariant="monospace">p</mi></mrow>&amp;MediumSpace;<mi>n</mi>&amp;MediumSpace;<mi>t</mi></mrow>\mathtt{top}\: n\: t</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.8373em; vertical-align: -0.22222em;" class="strut"></span><span class="mord"><span class="mord mathtt">t</span><span class="mord mathtt">o</span><span class="mord mathtt">p</span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord mathdefault">n</span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord mathdefault">t</span></span></span></span></span> returns the top <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>n</mi></mrow>n</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.43056em; vertical-align: 0em;" class="strut"></span><span class="mord mathdefault">n</span></span></span></span></span> layers of the given infinite tree <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>t</mi></mrow>t</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.61508em; vertical-align: 0em;" class="strut"></span><span class="mord mathdefault">t</span></span></span></span></span> as a finite binary tree.</li>
</ol>
<p>For example: </p>
<pre><code class="hljs"><span class="hljs-comment"># top 2 (layer_tree 25);;     </span>
- : int tree = <span class="hljs-keyword">Node</span> <span class="hljs-title">(25</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(26</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(26</span>, Empty, Empty))

<span class="hljs-comment"># top 3 (layer_tree 25);;</span>
- : int tree = <span class="hljs-keyword">Node</span> <span class="hljs-title">(25</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(26</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(27</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(27</span>, Empty, Empty)),
                         <span class="hljs-keyword">Node</span> <span class="hljs-title">(26</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(27</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(27</span>, Empty, Empty)))
</code></pre>
<ol>
<li><code>map : ('a -&gt; 'b) -&gt; 'a ltree -&gt; 'b ltree</code><br>
<span><span class="katex"><span class="katex-mathml"><math><mrow><mrow><mi mathvariant="monospace">m</mi><mi mathvariant="monospace">a</mi><mi mathvariant="monospace">p</mi></mrow>&amp;MediumSpace;<mi>f</mi>&amp;MediumSpace;<mi>t</mi></mrow>\mathtt{map}\: f\: t</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.91666em; vertical-align: -0.22222em;" class="strut"></span><span class="mord"><span class="mord mathtt">m</span><span class="mord mathtt">a</span><span class="mord mathtt">p</span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span style="margin-right: 0.10764em;" class="mord mathdefault">f</span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord mathdefault">t</span></span></span></span></span> maps all elements of the tree <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>t</mi></mrow>t</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.61508em; vertical-align: 0em;" class="strut"></span><span class="mord mathdefault">t</span></span></span></span></span> using the given function <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>f</mi></mrow>f</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.88888em; vertical-align: -0.19444em;" class="strut"></span><span style="margin-right: 0.10764em;" class="mord mathdefault">f</span></span></span></span></span>.</li>
<li><code>find : ('a -&gt; bool) -&gt; 'a ltree -&gt; 'a ltree</code><br>
<span><span class="katex"><span class="katex-mathml"><math><mrow><mrow><mi mathvariant="monospace">f</mi><mi mathvariant="monospace">i</mi><mi mathvariant="monospace">n</mi><mi mathvariant="monospace">d</mi></mrow>&amp;MediumSpace;<mi>p</mi>&amp;MediumSpace;<mi>t</mi></mrow>\mathtt{find}\: p\: t</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.80952em; vertical-align: -0.19444em;" class="strut"></span><span class="mord"><span class="mord mathtt">f</span><span class="mord mathtt">i</span><span class="mord mathtt">n</span><span class="mord mathtt">d</span></span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord mathdefault">p</span><span style="margin-right: 0.222222em;" class="mspace"></span><span class="mord mathdefault">t</span></span></span></span></span> returns the infinite subtree rooted at a node that satisfies the given predicate <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>p</mi></mrow>p</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.625em; vertical-align: -0.19444em;" class="strut"></span><span class="mord mathdefault">p</span></span></span></span></span>. Think about how to traverse the tree in order to make sure that every node is visited eventually.</li>
</ol>
<pre><code class="hljs"><span class="hljs-comment"># </span>
<span class="hljs-comment"># top 2 (find (fun y -&gt; y mod 2 = 0)  (layer_tree 25));; </span>
- : int tree = <span class="hljs-keyword">Node</span> <span class="hljs-title">(26</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(27</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(27</span>, Empty, Empty))

<span class="hljs-comment"># top 4 (find (fun y -&gt; y &gt; 9)  (layer_tree 1));;</span>
- : int tree =<span class="hljs-keyword">Node</span> <span class="hljs-title">(10</span>,
 <span class="hljs-keyword">Node</span> <span class="hljs-title">(11</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(12</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty)),
 <span class="hljs-keyword">Node</span> <span class="hljs-title">(12</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty))),
 <span class="hljs-keyword">Node</span> <span class="hljs-title">(11</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(12</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty)),
 <span class="hljs-keyword">Node</span> <span class="hljs-title">(12</span>, <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty), <span class="hljs-keyword">Node</span> <span class="hljs-title">(13</span>, Empty, Empty))))
</code></pre>
<p><em>Note: This is not a team exercise, you must submit your individual solution.</em>  </p></div>