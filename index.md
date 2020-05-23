---
layout: default
title: Black Hole Perturbation Toolkit
---
{% include head.html %}

<p>
 <h1 style="display:inline">GeneralRelativityTensors</h1> <span style="float:right;"><a href="https://bhptoolkit.org/mathematica-install.html" class = "code_btn">Install this package!</a></span>
</p>

A Mathematica package that provides a set of functions for performing coordinate-based tensor calculations with a focus on general relativity and black holes in particular.

## Example usage

The package has extensive documentation and tutorials. Below we give a few simple examples of the package in action.

### Defining the metric

All calculations using GeneralRelativityTensors require a metric with explicit values. You can define your own metric and there are also a range of useful build in metrics, i.e.,

```
g = ToMetric["Kerr"]
```
Other build in metrics include `"Minkowski"`, `"Schwarzschild"`, `"ReissnerNordstrom"`, "KerrNewman", etc (see the documentation of `ToMetric[]` for more details).

To view the components of any tensor you can use the `TensorValues[]` function. For example, with the metric defined above, `TensorValues[g]` returns

$$
\left(
\begin{array}{cccc}
 \frac{a^2 \sin ^2(\theta )-a^2+2 M r-r^2}{a^2 \cos ^2(\theta )+r^2} & 0 & 0 & -\frac{2 a
   M r \sin ^2(\theta )}{a^2 \cos ^2(\theta )+r^2} \\
 0 & \frac{a^2 \cos ^2(\theta )+r^2}{a^2-2 M r+r^2} & 0 & 0 \\
 0 & 0 & a^2 \cos ^2(\theta )+r^2 & 0 \\
 -\frac{2 a M r \sin ^2(\theta )}{a^2 \cos ^2(\theta )+r^2} & 0 & 0 & \frac{\sin
   ^2(\theta ) \left(\left(a^2+r^2\right)^2-a^2 \sin ^2(\theta ) \left(a^2-2 M
   r+r^2\right)\right)}{a^2 \cos ^2(\theta )+r^2} \\
\end{array}
\right) \nonumber
$$

### Defining tensors

The real power of the package comes from forming and manipulating tensors. Tensors are created using the `ToTensors[]` command. A few things to note:

- Tensors must be defined with a related metric, so indices can be raised or lower
- Negative indices are covariant, while positive indicies are contravariant

The following command defines a tensor $t_a$ on the metric $g$ above where all the components are functions of $r$.

```
t1 = ToTensor[{"NewTensor", "t"}, g, {f1[r], f2[r], f3[r], f4[r]}, {-a}]
```

The contravariant version, $t^a$, can be accessed via `t[a]`. As with the metric, the values of any tensor can be explicitly computed using `TensorValues[]`.

### Common tensors

Some common tensors are build in. For instance `ChristoffelSymbol[g, ActWith -> Simplify]` will compute the ChristoffelSymbols, $\Gamma^\alpha_{\beta\gamma}. The `ActWith` option means the Simplify command will be applied to each component before returning the result. Useful built in common tensors include: `RiemmannTensor, RicciTensor, RicciScalar, Einstein Tensor, KretschmannScalar, WeylTensor` and many more.

### Merging tensors

Another way to construct tensors is by merging other tensors using the command `MergeTensors[]`. As an example you can construct the Einstein tensor from the Ricci tensor, scalar and the metric via the commands:

```
g = ToMetric["Schwarzschild"]
ricT = RicciTensor[g]
ricS = RicciScalar[g]
einExpr = ricT[-a, -b] - g[-a], -b] ricS/2
```
The variable `einExpr` is not yet a tensor, but rather the sum and product of three different Tensors. These can be combined using the `MergeTensors`, e.g.,
```
einS = MergeTensors[einSExpr, {"EinsteinSchwarzschild", "G"}, ActWith -> Simplify]
```
This returns $G_{ab}$. You can explicitly then check that the Schwarzschild solution is a vacuum solution via `TensorValues[EinS]` which returns
```
{% raw %}{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}{% endraw %}
```

### More examples

The above just scratches the surface of what this package can do. Check the documentation for more details.