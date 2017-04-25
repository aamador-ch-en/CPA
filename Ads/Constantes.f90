! Modulo de constantes     
! File:   Constantes.f90
! Author: Abraham Amador
!
! Created on July 8, 2015, 12:19 PM
!
MODULE Constantes
    !Use Adsorcion
    Implicit None
    !Lista de constantes para funciones a y b usadas para calcular Z
    !Real,dimension(4)::y
    !Real::T,P
    Real,Parameter,Dimension(4)::Tc=(/ 190.56, 132.85, 304.12, 126.2 /)
    Real,Parameter,Dimension(4)::Pc=(/ 45.992, 34.94, 73.74, 33.98 /)
    Real,Parameter,Dimension(4)::w=(/ 0.011, 0.045, 0.225, 0.037 /)
    Real,Parameter,Dimension(4)::na=(/ 0.072, 0.608, 0.974, 25765.2 /)
    Real,Parameter,Dimension(4)::ma=(/15.68, 9.62, 34.04, 0.00 /)
    Real,Parameter,Dimension(4)::ba=(/ 6.48, 66.2, 14.04, 0.000928699 /)
    Real,Parameter,Dimension(4)::da=(/ 0.025, 0.03, 0.061, 0.00 /)
    Real,Parameter,Dimension(4)::ha=(/30.56, 48.16, 34.67, 30.0 /)
    Real,Parameter,Dimension(4)::ua=(/ 13.45, 14.84, 20.79, 0.0 /)
    Real,Parameter,Dimension(4)::ka=(/ 1.0, 1.0, 1.0, 1.13 /)
    Real,Parameter::c0=0.48,c1=1.574,c2=-0.176,eps=1,sig=0
    Double Precision,Parameter::omega=0.08664035,psi=0.42748023
    !Variables input para constante A
    !Variables input para A y B
    Contains
    Subroutine CA(y,T,At,ap,aij,D,n) 
          Implicit None
        !Double precision::psi 
	Double precision,intent(out)::At,D
        Double Precision,Dimension(4),intent(out)::ap
        Double Precision,Dimension(4,4),intent(out)::aij
        !Real,Dimension(4)::Tc,Pc,w
        !Real::c0,c1,c2
        Double precision,intent(in)::T,n
        Double precision,dimension(4),intent(in)::y
        Integer::i,j
        Do i=1,4 !Array de constantes a individuales
            ap(i)=(psi*(83.14*83.14*Tc(i)*Tc(i))/Pc(i))*((1+((c0+c1*w(i)+c2*w(i)*w(i))*(1-sqrt(Tc(i)/T))))**(2))         
        End do
        Do i=1,4
            Do j=1,4
                aij(i,j)=y(i)*y(j)*sqrt(ap(i)*ap(j))!Constantes duales de a
            End do 
        End Do 
        At=sum(sum(aij,dim=1)) !A total independiente de n
        D=At*n*n !A total dependiente de n
   End Subroutine CA
   Subroutine CB(y,Bt,bi,B,bij,n)
	Implicit none
	!Real,Dimension(4)::Tc,Pc
        Double precision, intent(in)::n
	Double Precision,Dimension(4),intent(out)::bi
	Double precision,dimension(4),intent(in)::y
	!Double precision::omega,bm
	Double precision,intent(out)::Bt,B
        Double precision,Dimension(4,4),intent(out)::bij
	Integer::i,j
        Do i=1,4 !Array de constantes b individuales
            bi(i)=omega*83.14*Tc(i)/Pc(i)
        End Do
        Do i=1,4 !Array constantes duales de b
            Do j=1,4
                bij(i,j)=0.5*(bi(i)+bi(j))
            End Do
        End Do
        BT=sum(bi*y) !B total independiente de n
        B=n*BT !b total dependiente de n
    End Subroutine CB
END MODULE Constantes
