!     
! File:   CPA.f90
! Author: Abraham Amador
!
! Created on 9 de octubre de 2015, 11:54 AM
!

MODULE CPA
    Use matriz
    Use VolStab
    Implicit None
    Real,Parameter,Dimension(4)::beta=(/ 0,0,0,0 /),epsi=(/0,0,0,0/)
    Contains
    Subroutine Solu(V,Xa,dXa,P,T,n,B,A,D,bi,ai,bij,aij,y)
        Double precision,intent(in)::B,A,D,n,P,T
        Double precision,dimension(4),intent(in)::bi,ai,y
        Double precision,dimension(4,4),intent(in)::bij,aij
        Double precision,dimension(4),intent(out)::Xa,dXa
        Double precision,dimension(4)::gXa
        Double precision,dimension(4,4)::Delta
        Double precision,intent(out)::V
        Double precision:: Vest,tol,Pcz,bep
        Double precision,dimension(4,5)::Jacx
        Real::opt
        Integer::i,j,im,jm,iter
        Logical::Singular
            Xa=(/1,1,1,1/)
            iter=1
            bep=sqrt(sum((beta+epsi)**2)) !Si los parametros de CPA son 0 se asume fraccion no asociada.
            Call Vol(n,T,Vest,P,D,B,Pcz,tol,Xa)
    If (bep.EQ.0) then
                Xa=(/1,1,1,1/)
                V=Vest
            Else
        Do while(iter<1000) !Rutina para obtener Xa
            Call Par(Delta,P,T,Vest,n,y,B,bij)
            V=Vest
            Do i=1,4 !definicion gXa
                gXa(i)=1+(n/V)*sum(Xa(i)*Delta(i,:))-(1/(Xa(i)))
            End do
            Do im=1,4 !Definicion matriz cuya determinante es el Jacobiano
                Do jm=1,4 
                    if (i.EQ.j) then
             Jacx(im,jm)=((n/V)*(Xa(i)+1E-6)*Delta(im,jm)-(1/(Xa(im)+1E-6))-(n/V)*(Xa(im)-1E-6)*Delta(im,jm)+(1/(Xa(im)-1E-6)))/2E-6
                    else
                        Jacx(im,jm)=((n/V)*(Xa(im)+1E-6)*Delta(im,jm)-(n/V)*(Xa(im)-1E-6)*Delta(im,jm))/2E-6
                    End if
                End Do
            End do
            !Vamos a intentar definir primero la parte de las variables de la matriz primero y luego añadir las soluciones
            !como array=Jacx(:,5) para meterlo a gsselm
            Jacx(:,5)=-gXa
            Call Gsselm(Jacx,4,dXa,SINGULAR) !Cuchareada del Dr. Lira
            opt=sqrt(sum(dxa**2))
            Xa=Xa+dxa
            iter=1+iter
        End Do
    End if
    End Subroutine Solu
    Subroutine Par(Delta,P,T,Vnec,n,y,B,bij)!Parametros para la CPA, se usa dentro de Solu
        Double precision,dimension(4,4),intent(out)::Delta
        Double precision::eta
        Double precision,Dimension(4)::gXa !gXa es la funcion a optimizar para obtener Xa
        Double precision,dimension(4,4)::betm,epsim !Coeficientes assoc 
        Double precision,intent(in)::P,T,Vnec,B,n
        Double precision,dimension(4,4),intent(in)::bij
        Double precision,dimension(4),intent(in)::y
        integer::i,j
        Real::opt !tolerancia para solucion Xa
        eta=1.9*B*n/(4*Vnec)
        Do i=1,4
            Do j=1,4
            epsim(i,j)=0.5*(epsi(i)+epsi(j))
            betm(i,j)=sqrt(beta(i)*beta(j))
            End do
        End Do
        Do i=1,4
            Do j=1,4
                Delta(i,j)=(exp(epsim(i,j)/(8.314*T))*bij(i,j)*betm(i,j))/(1-eta)!Matriz de Delta de mezclado regla CR-1
            End Do
        End Do
    End subroutine Par

END MODULE CPA
