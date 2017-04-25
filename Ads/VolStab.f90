!     
! File:   VolStab.f90
! Author: Abraham Amador
!
! Created on 9 de octubre de 2015, 10:06 AM
!

MODULE VolStab
    Contains 
    Subroutine Vol(n,T,Vest,P,D,B,Pcz,tol,Xa)
        Implicit None
        Double precision, intent(in)::n,T,P,D,B
        Double precision, intent(out)::Vest,Pcz,tol
        Double precision:: Vast
        Double precision::Hv,dHv
        Double precision::Ppa
        Double precision,Dimension(4),intent(in)::Xa
        integer::i
        tol=1
        Vast=n*8.314*T/(P*1E5)
        i=1
        Do while (i<1000)
        Ppa=P
        tol=abs(Pca(n,B,D,T,Vast,Xa)-Ppa) !Primero tolerancia para saber si el while se hace
        Hv=P-Pca(n,B,D,T,Vast,Xa) !Definimos FO
        dHv=(-Pca(n,B,D,T,(Vast+1E-6),Xa)+Pca(n,B,D,T,(Vast-1E-6),Xa))/2E-6 !Su derivada numerica
        Vast=Vast-(Hv/dHv)!*1E-5 !Metodo de Newton
        i=i+1
        Vest=Vast*1E-6
        Pcz=Pca(n,B,D,T,Vast,Xa)
        End Do
    End subroutine Vol

    Double precision Function Pca(n,B,D,T,V,Xa)
    Double precision:: n,B,D,T,V
    Double precision,dimension(4),intent(in)::Xa
    Pca=83.14*T*((n/V)-(-((n*B)/(V*(V-B)))+((D/T)/(83.14*V*(V+B)))+((1/(2*V))*(1+(0.475*B)/((V-0.475*B)**2))*(n*sum(1-Xa)))))
    End function Pca

END MODULE VolStab
