! Programa Maestro    
! File:   Adsorcion.f90
! Author: Abraham Amador
!
! Created on July 8, 2015, 10:20 AM
!
Program Datos
    use Constantes
    use CPA
    use VolStab
    use Fugac
    Implicit none
    Double precision:: T,P
    Double precision,dimension(4)::y
    Double Precision::n,Pcz,tol,Vest,At,Bt,D,B,V,Z
    Double precision,dimension(4)::nads,bi,ap,Xa,dXa,FSRK,FCPA,Fu,phi,lnp
    Double precision,dimension(4,4)::aij,bij
    Double precision,dimension(4)::Xb=(/1,1,1,1/)
    Integer::i
    Print*, "Temperature of the system (Kelvin)" !Imprimir instrucciones input datos
    read(*,*) T
    Print*,"Pressure (Bar)"
    Read(*,*) P
    Print*, "Mole fraction CH4"
    read(*,*) y(1)
    Print*, "Mole fraction CO"
    Read(*,*) y(2)
    Print*, "Mole fraction CO2"
    Read(*,*) y(3)
    Print*, "Mole fraction N2"
    Read(*,*) y(4)
    Print*, "Total moles"
    read(*,*) n
    Call CA(y,T,At,ap,aij,D,n)
    Call CB(y,BT,bi,B,bij,n)
    
    Print*, "Constants At, D, Bt, B"
    Write(*,*) At
    Write(*,*) D
    Write(*,*) BT
    Write(*,*) B
    
    Print*, "P calculated, error, V SRK"
    Call Vol(n,T,Vest,P,D,B,Pcz,tol,Xb)
    Write(*,*) Pcz
    Write(*,*) tol
    Write(*,*) Vest
    
    Call Solu(V,Xa,dXa,P,T,n,B,At,D,bi,ap,bij,aij,y)
    Print*, "Volume"
    Write(*,*) V
    Print*, "Non associated components"
    Write(*,*) Xa
        
    Call Fugg(At,Aij,ap,B,Bij,bi,D,n,y,V,FSRK,FCPA,Fu,P,T,Xa,Z,phi,lnp)
    Print*, "Z"
    Write(*,*) Z
    Print*, "FSRK"
    Write(*,*) FSRK
    Print*, "FCPA"
    Write(*,*) FCPA
    Print*, "phi"
    Write(*,*) phi
    Print*, "Fugacity(bar)"
    Write(*,*) Fu
End Program 
