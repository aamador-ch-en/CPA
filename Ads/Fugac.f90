!     
! File:   Fugac.f90
! Author: Abraham Amador
!
! Created on 6 de noviembre de 2015, 12:40 PM
!

MODULE Fugac
    Implicit none
    Contains 
    Subroutine Fugg(A,Aij,ai,B,Bij,bi,D,n,y,V,FSRK,FCPA,Fu,P,T,Xa,Z,phi,lnp)
        Double precision,dimension(4,4),intent(in)::Aij,Bij
        Double precision,intent(in)::n,P,T
        Double precision,dimension(4),intent(out)::Fu,FCPA,FSRK,phi,lnp
        Double precision,intent(in)::A,B,D,V
        Double precision,dimension(4),intent(in)::y,ai,bi
        Double precision,dimension(4)::fBi,fDi,Xa !Parametros de la ecuacion, estan en pagina 290 del libro
        Double precision::Fd,fv,fbm,gb,fb,fn,g,FF
        Double precision,dimension(4)::dlng
        Double precision,dimension(4)::arbi
        Double precision,intent(out)::Z
        Integer::i,j
        Do i=1,4
            arbi(i)=sum(n*y(i)*bij(i,:))
        End Do
        Do i=1,4
            fBi(i)=(2*arbi(i)-B)/n
            fDi(i)=sum(2*(y(i)*n*Aij(i,:)))
        End Do
        g=log(1+(B/V))
        FF=(1/(8.314*B))*(log(1+(B/V)))
        Fd=(-1/T)*FF
        fv=-(1/8.314)*(1/(V*(V+B)))
        fbm=(-1/B)*(FF+V*fv)
        gb=-1/(V-B)
        Fb=-n*gb-(D/T)*fbm
        Fn=-g
        Do i=1,4
            FSRK(i)=Fn+Fb*fDi(i)+Fd*fBi(i)
        End Do
        Do i=1,4
            dlng(i)=0.475*V*((1/(V-0.475*B))**2)*fBi(i)*(1-1.9*(B/(4*V)))
        End do
        Do i=1,4
            Fcpa(i)=sum(log(Xa))-0.5*sum(y*n)*sum(1-Xa)*dlng(i)
        End Do
        Z=1E5*P*V/(n*8.314*T)
        Do i=1,4
            lnp(i)=1E-6*(FCPA(i)+FSRK(i))/(8.314*T)
        End do
        Do i=1,4
            phi(i)=exp(lnp(i))
        End do
        Do i=1,4
            Fu(i)=P*y(i)*phi(i)/Z
        End Do
    End subroutine Fugg
END MODULE Fugac
