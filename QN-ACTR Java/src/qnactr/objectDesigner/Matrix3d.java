package qnactr.objectDesigner;

public class Matrix3d {

	public double Matrix[][]= new double [4][4];
	
	
	//for 3D transformation
	//based on http://www.inversereality.org/tutorials/graphics%20programming/3dwmatrices.html
	
	public void SetAsIdentityMatrix()  //set self as identity matrix
	{ 
	  SetAllZero();
	  Matrix[0][0]=Matrix[1][1]=Matrix[2][2]=Matrix[3][3]=1.0;
	}
	
	
	
	public void SetAllZero(){ //set all zero
	  for(int i=0;i<4;i++)
	  { 
	    for(int j=0;j<4;j++) {
	      Matrix[i][j]=0.0;
	    }
	  }
	}
	
	/*
public void MatrixCopy(Matrix3d NewM){  // this is called Copy, but it means self Matrix * NewM
  Matrix3d temp = new Matrix3d();   
  int i,j;
  for(i=0;i<4;i++)
   {for(j=0;j<4;j++)
     {
    temp.Matrix[i,j] = (Matrix[i,0]*NewM.Matrix[0,j])+(Matrix[i,1]*NewM.Matrix[1,j])   +   (Matrix[i,2]*NewM.Matrix[2,j])+(Matrix[i,3]*NewM.Matrix[3,j]);
     }
   }
 for(i=0;i<4;i++)
  {Matrix[i,0] = temp.Matrix[i,0];
   Matrix[i,1] = temp.Matrix[i,1];
   Matrix[i,2] = temp.Matrix[i,2];
   Matrix[i,3] = temp.Matrix[i,3];
   }
}
	 */
	
	public Matrix3d MatrixMultiply(Matrix3d M1,Matrix3d M2)  // set M1  =  M1 * M2
	{ 
	  Matrix3d temp = new Matrix3d();
	  temp.SetAllZero();
	  int i,j;
	  for (i=0;i<4;i++)
	  { 
	    for(j=0;j<4;j++)
	    {
	      temp.Matrix[i][ j] =
	          (M1.Matrix[i][ 0] * M2.Matrix[0][ j]) + (M1.Matrix[i][ 1] * M2.Matrix[1][ j]) +
	          (M1.Matrix[i][ 2] * M2.Matrix[2][ j]) + (M1.Matrix[i][ 3] * M2.Matrix[3][ j]);
	    }
	  }
	  
	  return temp;
	}
	
	public String ToString(){
	  int i,j;
	  String return_string = "(";
	  for(i=0;i<4;i++)
	  {for(j=0;j<4;j++)
	  {
	    return_string = return_string + Matrix[i][j] + " ";
	  }
	  return_string += ", ";
	  }
	  return_string += ")";
	  return return_string;
	}

	
	}

