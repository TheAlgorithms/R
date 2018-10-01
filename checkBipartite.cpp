//Check whether a graph is bipartite or not

#include<bits/stdc++.h>
#define max 10000                                                   // Limit of vertices

int mat[max][max];                                                  //Adjacency Matrix (to be taken as input)
int bfsQ[max];                                                      //Queue for BFS
int r = -1;
int f = 0;
bool bfsVisited[max];                                               //Visited array for vertices
bool color[max];                                                    //Color array for vertices

void makeBFS(int v, int n){                                         //make bfs and assign alternate colors to vertices
  for(int i=0;i<n;i++){
    if(mat[v][i] && !bfsVisited[i]){
      bfsQ[++r] = i;
      color[i] = !color[v];
    }
  }
    if(f <= r){
      bfsVisited[bfsQ[f]] = true;
      makeBFS(bfsQ[f++], n);
  }
}

bool checkBipartite(int n){                                         //check bipartite using 2-colored theorem
  bool result = true;
  int i;
  for(i=0;i<max;i++){
      bfsQ[i]=0;
      bfsVisited[i]=false;
  }
  color[0] = true;
  makeBFS(0,n);
  for(int i=0;i<n;i++){
    for(int j=0;j<n;j++){
      if(mat[i][j] == 1 && color[i] == color[j]){
        result = false;
        break;
      }
    }
  }
  return result;
}

using namespace std;

int main(){
  int n;
  cout<<"Enter number of vertices : ";
  cin>>n;
  cout<<"Enter adjacency matrix: \n";
  for(int i=0;i<n;i++){
    for(int j=0;j<n;j++){
      cin>>mat[i][j];
    }
  }
  bool ch = checkBipartite(n);
  cout<<boolalpha<<"\n Is the graph a Bipartite Graph? : \t"<<ch<<endl;
  return 0;
}
