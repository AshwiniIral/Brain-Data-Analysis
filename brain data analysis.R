fi= "fdg.dyn"
fa="Attn"
faq="FDG.acqtimes90"

tx=matrix(scan(faq,skip=2),ncol=2,byrow=T) # 31 2
tx = tx/60 #convert to min
tx

#reading fdg.dyn data
x = readBin(fi, n = 128*128*35*31, numeric(),endian = "big",size = 4)
x = array(x,c(128,128,35,31)) 
dim(X)  #128 128 35 31   images,slice,timepoint

#reading attenuation data
xa = readBin(fa, n = 128*128*35, numeric(),endian = "big",size = 4)
xa = array(xa,c(128,128,35)) 
dim(xa) # 128 128 35

# plot of time course
y = apply(x,c(4),mean)
plot(y)

# setting the grey scale for the plot
gr = grey(c(1:128)/128) 
n1 = 128 ; n2 = 128 ; n3 = 35
par(mfrow=c(3,3))

# mid-plane slices

# Early
xs = apply(x,c(1,2,3),sum)# whole image
image(xs[,rev(1:n2),n3/2],col=gr,axes=F,main="Transverse") # Eye looking up so reverse. Coronal slice from top
image(xs[,n2/2,],col=gr,axes=F,main="Coronal") # Front view slice. Should be able to see neck area at the bottom.
image(xs[n1/2,rev(1:n2),],col=gr,axes=F,main="Sagittal") # Sagittal scan is from the side.

xs = apply(xa,c(1,2,3),sum) # 128 128 35  # whole image
image(xs[,rev(1:n2),round(n3/2)],col=gr,axes=F,main="Transverse") # Head holder in position can be determined from this.
image(xs[,n2/2,],col=gr,axes=F,main="Coronal") # Front view slice. Should be able to see neck area at the bottom.
image(xs[n1/2,rev(1:n2),],col=gr,axes=F,main="Sagittal") # Sagittal scan is from the side.We can see the head holder in this as well.

xe = apply(x[,,,2:9],c(1,2,3),sum)# early frame 0-3min
xi = apply(x[,,,15:19],c(1,2,3),sum)# intermediate 10-30min
xl = apply(x[,,,26:29],c(1,2,3),sum)# late 60-80min

par(mfrow=c(3,3))

#mid-plane slices

# Head holder in position can be determined from this
image(xe[,rev(1:n2),n3/2],col=gr,axes=F,main="Transverse",ylab="Early")
image(xe[,n2/2,],col=gr,axes=F,main="Coronal")
image(xe[n1/2,rev(1:n2),],col=gr,axes=F,main="Sagittal")

# Front view slice. Should be able to see neck area at the bottom
image(xi[,rev(1:n2),n3/2],col=gr,axes=F,main="",ylab="Intermediate")
image(xi[,n2/2,],col=gr,axes=F,main="")
image(xi[n1/2,rev(1:n2),],col=gr,axes=F,main="")

# Sagittal scan is from the side.We can see the head in this as well
image(xl[,rev(1:n2),n3/2],col=gr,axes=F,main="",ylab="Later")
image(xl[,n2/2,],col=gr,axes=F,main="")
image(xl[n1/2,rev(1:n2),],col=gr,axes=F,main="")


# at time stamp t=24 ; without converting to xs
t=24
image(x[,,round(n3/2),t],col=grey(c(1:128)/128),main="Transverse mid-plane")
image(x[,round(n2/2),,t],col=grey(c(1:128)/128),main="Coronal mid-plane")
image(x[round(n1/2),,,t],col=grey(c(1:128)/128),main="Saggital mid-plane")

image(xa[,,round(n3/2)],col=grey(c(1:128)/128),main="Transverse mid-plane")
image(xa[,round(n2/2),],col=grey(c(1:128)/128),main="Coronal mid-plane")
image(xa[round(n1/2),,],col=grey(c(1:128)/128),main="Saggital mid-plane")
 
#------------------------------------------------------------------------------------
# principal component analysis

par(mfrow=c(2,2))
#converting to data matrix
X = matrix(c(x),ncol=31) #col1 =  time point 1 
dim(X) # 573440 31 = 128*18*35 31

o = prcomp(X,scale=TRUE)
plot(o)
#From the plot of o, we can concule the 1st principal component is enough.
#tx[,1] = starting time of all time frames.
matplot(tx[,1],o$rotation[,1:3],type="l",xlab="Time",ylab="")

z = array(c(o$x),c(128,128,35,31)) #Convert back to image dimensions from matrix dimensions.
pca1 = z[,,,1] # PC1
pca2 = z[,,,2] # PC2
pca3 = z[,,,3] # PC3

par(mfrow=c(3,3))

image(pca1[,rev(1:n2),n3/2],col=gr,axes=F,main="Transverse",ylab="1")
image(pca1[,n2/2,],col=gr,axes=F,main="Coronal")
image(pca1[n1/2,rev(1:n2),],col=gr,axes=F,main="Sagittal")

image(pca2[,rev(1:n2),n3/2],col=gr,axes=F,main="",ylab="2")
image(pca2[,n2/2,],col=gr,axes=F,main="")
image(pca2[n1/2,rev(1:n2),],col=gr,axes=F,main="")

image(pca3[,rev(1:n2),n3/2],col=gr,axes=F,main="",ylab="3")
image(pca3[,n2/2,],col=gr,axes=F,main="")
image(pca3[n1/2,rev(1:n2),],col=gr,axes=F,main="")