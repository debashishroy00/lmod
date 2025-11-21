VERSION 5.00
Begin VB.Form StartForm
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Start"
   ClientHeight    =   1500
   ClientLeft      =   2760
   ClientTop       =   3750
   ClientWidth     =   4050
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1500
   ScaleWidth      =   4050
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.CommandButton cmdClose
      Caption         =   "Close"
      Height          =   375
      Left            =   2640
      TabIndex        =   3
      Top             =   960
      Width           =   1215
   End
   Begin VB.CommandButton cmdOpen
      Caption         =   "Open"
      Height          =   375
      Left            =   1320
      TabIndex        =   2
      Top             =   960
      Width           =   1215
   End
   Begin VB.CommandButton cmdNew
      Caption         =   "New"
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   960
      Width           =   1095
   End
   Begin VB.TextBox txtID
      Height          =   285
      Left            =   1320
      TabIndex        =   0
      Top             =   360
      Width           =   2535
   End
   Begin VB.Label Label1
      Caption         =   "Client ID"
      BeginProperty Font
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   360
      Width           =   1095
   End
End
Attribute VB_Name = "StartForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub cmdNew_Click()
  Dim objClient As New Client
  Dim frm As New ClientEdit

  Set frm.Client = objClient
  frm.Show vbModal
  Set frm = Nothing
End Sub

Private Sub cmdOpen_Click()
  Dim objClient As Client
  Dim frm As New ClientEdit

  On Error Resume Next
  Set objClient = GetClient(CLng(txtID.Text))

  If objClient Is Nothing Then
    MsgBox "Client ID not found"
  Else
    Set frm.Client = objClient
    frm.Show vbModal
  End If
  Set frm = Nothing
End Sub

Private Sub cmdClose_Click()
  Unload Me
End Sub
