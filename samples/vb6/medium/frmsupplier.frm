VERSION 5.00
Object = "{CDE57A40-8B86-11D0-B3C6-00A0C90AEA82}#1.0#0"; "MSDATGRD.OCX"
Begin VB.Form frmsupplier
   Caption         =   "Supplier Management"
   ClientHeight    =   8145
   ClientLeft      =   120
   ClientTop       =   450
   ClientWidth     =   15375
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   8145
   ScaleWidth      =   15375
   WindowState     =   2  'Maximized
   Begin VB.ComboBox Combo1
      Height          =   315
      Left            =   2400
      TabIndex        =   15
      Top             =   6840
      Width           =   2415
   End
   Begin VB.CommandButton Command5
      Caption         =   "Reset"
      Height          =   495
      Left            =   11520
      TabIndex        =   14
      Top             =   3720
      Width           =   1575
   End
   Begin VB.CommandButton Command4
      Caption         =   "Edit"
      Height          =   495
      Left            =   11520
      TabIndex        =   13
      Top             =   2760
      Width           =   1575
   End
   Begin VB.CommandButton Command3
      Caption         =   "Delete"
      Height          =   495
      Left            =   11520
      TabIndex        =   12
      Top             =   1920
      Width           =   1575
   End
   Begin VB.CommandButton Command2
      Caption         =   "Add"
      Height          =   495
      Left            =   11520
      TabIndex        =   11
      Top             =   1080
      Width           =   1575
   End
   Begin VB.TextBox Text4
      Height          =   375
      Left            =   2400
      TabIndex        =   7
      Top             =   2880
      Width           =   2415
   End
   Begin VB.TextBox Text3
      Height          =   375
      Left            =   2400
      TabIndex        =   6
      Top             =   2280
      Width           =   2415
   End
   Begin VB.TextBox Text2
      Height          =   375
      Left            =   2400
      TabIndex        =   5
      Top             =   1680
      Width           =   2415
   End
   Begin VB.TextBox Text1
      Height          =   375
      Left            =   2400
      TabIndex        =   4
      Top             =   1080
      Width           =   2415
   End
   Begin MSDataGridLib.DataGrid DataGrid1
      Height          =   4095
      Left            =   5760
      TabIndex        =   3
      Top             =   1080
      Width           =   4935
      _ExtentX        =   8705
      _ExtentY        =   7223
      _Version        =   393216
      HeadLines       =   1
      RowHeight       =   15
      BeginProperty HeadFont {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ColumnCount     =   2
      BeginProperty Column00
         DataField       =   ""
         Caption         =   ""
         BeginProperty DataFormat {6D835690-900B-11D0-9484-00A0C91110ED}
         EndProperty
      EndProperty
      BeginProperty Column01
         DataField       =   ""
         Caption         =   ""
         BeginProperty DataFormat {6D835690-900B-11D0-9484-00A0C91110ED}
         EndProperty
      EndProperty
      SplitCount      =   1
      BeginProperty Split0
         BeginProperty Column00
         EndProperty
         BeginProperty Column01
         EndProperty
      EndProperty
   End
   Begin VB.Label Label6
      Caption         =   "Search by Name:"
      Height          =   255
      Left            =   480
      TabIndex        =   10
      Top             =   6840
      Width           =   1575
   End
   Begin VB.Label Label5
      Caption         =   "Email ID:"
      Height          =   255
      Left            =   480
      TabIndex        =   9
      Top             =   2880
      Width           =   1215
   End
   Begin VB.Label Label4
      Caption         =   "Mobile No:"
      Height          =   255
      Left            =   480
      TabIndex        =   8
      Top             =   2280
      Width           =   1215
   End
   Begin VB.Label Label3
      Caption         =   "Location:"
      Height          =   255
      Left            =   480
      TabIndex        =   2
      Top             =   1680
      Width           =   1215
   End
   Begin VB.Label Label2
      Caption         =   "Supplier Name:"
      Height          =   255
      Left            =   480
      TabIndex        =   1
      Top             =   1080
      Width           =   1455
   End
   Begin VB.Label Label1
      Alignment       =   2  'Center
      Caption         =   "SUPPLIER MANAGEMENT"
      BeginProperty Font
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   5760
      TabIndex        =   0
      Top             =   240
      Width           =   4935
   End
End

Private Sub Command2_Click()
    ' Add new supplier
    Dim rs As New ADODB.Recordset
    rs.Open "select * from suppliers", con, adOpenDynamic, adLockOptimistic

    If Text1.Text = "" Or Text2.Text = "" Or Text3.Text = "" Or Text4.Text = "" Then
        MsgBox "All fields are required!", vbExclamation
        Exit Sub
    End If

    rs.AddNew
    rs.Fields("sname") = Text1.Text
    rs.Fields("splace") = Text2.Text
    rs.Fields("smobno") = Text3.Text
    rs.Fields("smailid") = Text4.Text
    rs.Update
    rs.Close

    MsgBox "Supplier added successfully!", vbInformation
    Call loadgrid
    Call clearfields
End Sub

Private Sub Command3_Click()
    ' Delete supplier
    Dim rs As New ADODB.Recordset
    Dim response As Integer

    response = MsgBox("Are you sure you want to delete this supplier?", vbYesNo + vbQuestion)

    If response = vbYes Then
        rs.Open "delete from suppliers where sid=" & DataGrid1.Columns(0).Text, con
        MsgBox "Supplier deleted successfully!", vbInformation
        Call loadgrid
        Call clearfields
    End If
End Sub

Private Sub Command4_Click()
    ' Edit supplier
    Dim rs As New ADODB.Recordset
    rs.Open "select * from suppliers where sid=" & DataGrid1.Columns(0).Text, con, adOpenDynamic, adLockOptimistic

    If Not rs.EOF Then
        rs.Fields("sname") = Text1.Text
        rs.Fields("splace") = Text2.Text
        rs.Fields("smobno") = Text3.Text
        rs.Fields("smailid") = Text4.Text
        rs.Update
        MsgBox "Supplier updated successfully!", vbInformation
        Call loadgrid
        Call clearfields
    End If
    rs.Close
End Sub

Private Sub Command5_Click()
    ' Reset fields
    Call clearfields
End Sub

Private Sub Combo1_Click()
    ' Search by supplier name
    Dim rs As New ADODB.Recordset
    rs.Open "select * from suppliers where sname like '%" & Combo1.Text & "%'", con
    Set DataGrid1.DataSource = rs
End Sub

Private Sub DataGrid1_Click()
    ' Populate fields when grid row is clicked
    Text1.Text = DataGrid1.Columns(1).Text
    Text2.Text = DataGrid1.Columns(2).Text
    Text3.Text = DataGrid1.Columns(3).Text
    Text4.Text = DataGrid1.Columns(4).Text
End Sub

Private Sub Form_Load()
    Call loadgrid
    Call fillcombo
End Sub

Private Sub loadgrid()
    ' Load supplier data into grid
    Dim rs As New ADODB.Recordset
    rs.Open "select * from suppliers", con
    Set DataGrid1.DataSource = rs
End Sub

Private Sub clearfields()
    Text1.Text = ""
    Text2.Text = ""
    Text3.Text = ""
    Text4.Text = ""
End Sub

Private Sub fillcombo()
    ' Fill search combo with supplier names
    Dim rs As New ADODB.Recordset
    rs.Open "select sname from suppliers", con
    While Not rs.EOF
        Combo1.AddItem rs.Fields("sname")
        rs.MoveNext
    Wend
    rs.Close
End Sub
