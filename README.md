# Bibmin


```mermaid
sequenceDiagram
	participant US as User
	participant UI as User Interface
	participant AS as API Server
	participant ML as Minify Logic
	US ->>+ UI: Input BibTeX String
	UI ->> UI: Load Default Schema
	UI ->>+ AS: Query Schema with BibTeX String
	AS ->>+ ML: BibTeX String with Optional Fields
	ML ->> ML: Parse, Transform, and Pretty Print
	ML -->>- AS: Return Minified BibTeX
	AS -->>- UI: Response Minified BibTeX
	UI -->>- US: View Minified BibTeX


```

