# `supervised_peel()` generates correct values

    Code
      vapply(x$orig, sum, 0.1)
    Output
              sdev     rotation            x        basis  single.vals 
       3.45918e+00 -3.34443e+00  7.25024e-14  1.34266e-13  3.44184e+01 

---

    Code
      vapply(x$weighted, sum, 0.1)
    Output
              sdev     rotation            x        basis  single.vals 
       4.54388e-01  3.61219e+01 -4.28478e-14  3.07812e+01  4.52111e+00 

---

    Code
      vapply(x$unweighted, sum, 0.1)
    Output
              sdev     rotation            x        basis  single.vals 
       4.54388e-01  1.17052e+00 -6.57716e-14  3.07812e+01  4.52111e+00 

