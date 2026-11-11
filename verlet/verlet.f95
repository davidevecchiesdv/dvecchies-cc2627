PROGRAM verlet
  IMPLICIT NONE
  REAL,PARAMETER :: fx=0, fy=0.1, fz=0, tau=0.2, m=1
  REAL :: tk, xk, yk, zk, vk, vkx, vky, vkz, vk1y, y1k
  INTEGER :: i, n
  xk=0; yk=0; zk=0; vkx=0; vky=0; vkz=0; n=600; vk1y=0; y1k=0
  DO i=1,n
  xk=xk+(tau*vkx)+(tau**2)*(fx/(2*m))
  yk=yk+(tau*vky)+(tau**2)*(fy/(2*m))
  zk=zk+(tau*vkz)+(tau**2)*(fz/(2*m))
  PRINT*,xk, yk, zk
  vkx=vkx+(tau/(m))*fx
  vky=vky+(tau/(m))*fy
  vkz=vkz+(tau/(m))*fz
  ENDDO
  PRINT*, vkx, vky, vkz
  PRINT*, "VERIFICATION"
  vk1y=0+(fy/m)*tau*n
  y1k=0+0+0.5*(fy/m)*(tau*n)**2
  PRINT*, "Velocity"
  PRINT*, vkx, vky, vkz
  PRINT*, "Calculated"
  PRINT*, vk1y
  PRINT*, "Position"
  PRINT*, xk, yk, zk
  PRINT*, "Calculated" 
  PRINT*, y1k
END PROGRAM verlet  
