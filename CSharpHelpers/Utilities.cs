using System;
using MicrosoftResearch.Infer;
using MicrosoftResearch.Infer.Models;
using MicrosoftResearch.Infer.Maths;
using MicrosoftResearch.Infer.Distributions;
using MicrosoftResearch.Infer.Collections;




public class Utilities {
    public static double StdDeviation(object dist) {
        var v = ((CanGetVariance<double>)dist).GetVariance();
        return System.Math.Sqrt(v);
    }

    public static double[] DiscreteProbs(object d) {
        var counts = ((Discrete)d).GetProbs();
        return counts.ToArray();
    }

    public static double[] DirichletCounts(object d) {
        var counts = ((Dirichlet)d).PseudoCount;
        return counts.ToArray();
    }

    public static double Sum(double[] a) {
        double s = 0;
        for (int i = 0; i < a.Length; i++) {
            s += a[i];
        }
        return s;
    }

    public static PositiveDefiniteMatrix DiagonalPDMatrix(double[] v) {
        var mat = PositiveDefiniteMatrix.Identity(v.Length);
        mat.SetDiagonal(Vector.FromArray(v));
        return mat;
    }

    public static object IndexDist(object dists, int i) {
        var m = dists.GetType().GetMethod("get_Item");
        return m.Invoke(dists, new object[] { (object)i });
    }

    public static double HashMax(double[] a) {
        double m = Double.NegativeInfinity;
        for (int i = 0; i < a.Length; i++) {
            m = System.Math.Max(a[i], m);
        }
        return m;
    }
    public static int ArgMax(double[] a) {
        int m = 0;
        for (int i = 0; i < a.Length; i++) {
            if (a[i] > a[m])
                m = i;
        }
        return m;
    }

    public static int ArgMin(double[] a) {
        int m = 0;
        for (int i = 0; i < a.Length; i++) {
            if (a[i] < a[m])
                m = i;
        }
        return m;
    }

    public static object[] AsArray(object o) {
        var a = (o as ConvertibleToArray).ToArray();
        var os = new object[a.GetLength(0)];
        for (int i = 0; i < os.Length; i++)
            os[i] = a.GetValue(i);
        return os;
    }

    public static Range GetValueRange(Variable v) {
        return v.GetValueRange(throwIfMissing: false);
    }
    public static T SetValueRange<T>(T v, Range r) where T : Variable {
        if (r != null) v.SetValueRange(r);
        return v;
    }

    public static int ValueCount<T>(T?[] vs) where T : struct {
        var c = 0;
        for (var i = 0; i < vs.Length; i++) {
            if (vs[i].HasValue) c++;
        }
        return c;
    }
    public static T[] Values<T>(T?[] vs) where T : struct {
        var ws = new T[ValueCount(vs)];
        var h = 0;
        for (var i = 0; i < vs.Length; i++) {
            if (vs[i].HasValue) ws[h++] = vs[i].Value;
        }
        return ws;
    }
    public static int[] Indices<T>(T?[] vs) where T : struct {
        var ws = new int[ValueCount(vs)];
        var h = 0;
        for (var i = 0; i < vs.Length; i++) {
            if (vs[i].HasValue) ws[h++] = i;
        }
        return ws;
    }
    public static U Let<T, U>(T x, Func<T, U> f) {
        return f(x);
    }
    public static T[] ForLoop<T>(int n, Func<int, T> f) {
        var a = new T[n];
        for (int i = 0; i < n; i++) {
            a[i] = f(i);
        }
        return a;
    }

}




