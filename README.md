apiVersion: apps/v1
kind: Deployment
metadata:
  name: inventory-management-service
spec:
  replicas: 3
  selector:
    matchLabels:
      app: inventory-management-service
  template:
    metadata:
      labels:
        app: inventory-management-service
    spec:
      containers:
        - name: inventory-management-service
          image: 975049965288.dkr.ecr.us-east-1.amazonaws.com/commerce-internal-assignment:inventory-management-service
          ports:
            - containerPort: 8081

---
apiVersion: v1
kind: Service
metadata:
  name: inventory-management-service
spec:
  selector:
    app: inventory-management-service
  ports:
    - protocol: TCP
      portapiVersion: apps/v1
kind: Deployment
metadata:
  name: inventory-management-service
spec:
  replicas: 3
  selector:
    matchLabels:
      app: inventory-management-service
  template:
    metadata:
      labels:
        app: inventory-management-service
    spec:
      containers:
        - name: inventory-management-service
          image: 975049965288.dkr.ecr.us-east-1.amazonaws.com/commerce-internal-assignment:inventory-management-service
          ports:
            - containerPort: 8081

---
apiVersion: v1
kind: Service
metadata:
  name: inventory-management-service
spec:
  selector:
    app: inventory-management-service
  ports:
    - protocol: TCP
      port: 81
      targetPort: 8081
  type: LoadBalancer
: 81
      targetPort: 8081
  type: LoadBalancer


apiVersion: apps/v1
kind: Deployment
metadata:
  name: inventory-management-service
spec:
  replicas: 3
  selector:
    matchLabels:
      app: inventory-management-service
  template:
    metadata:
      labels:
        app: inventory-management-service
    spec:
      containers:
        - name: inventory-management-service
          image: 975049965288.dkr.ecr.us-east-1.amazonaws.com/commerce-internal-assignment:inventory-management-service
          ports:
            - containerPort: 8081

---
apiVersion: v1
kind: Service
metadata:
  name: inventory-management-service
spec:
  selector:
    app: inventory-management-service
  ports:
    - protocol: TCP
      port: 81
      targetPort: 8081
  type: LoadBalancer
