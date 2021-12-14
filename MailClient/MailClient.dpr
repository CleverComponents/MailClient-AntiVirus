program MailClient;

uses
  Forms,
  main in 'main.pas' {MainForm},
  MessagePersister in 'MessagePersister.pas',
  AccountsForm in 'AccountsForm.pas' {frmAccounts},
  MessageSourceForm in 'MessageSourceForm.pas' {frmMessageSource},
  Progress in 'Progress.pas' {frmProgress},
  RVQuoteText in '..\RVQuoteText.pas',
  clRVSendDialog in '..\clRVSendDialog.pas' {frmComposeHTMLEMail},
  clRVSendDialogEx in '..\clRVSendDialogEx.pas',
  clRVSendMail in '..\clRVSendMail.pas',
  ClamAV in 'ClamAV.pas',
  libpclamav in 'libpclamav.pas',
  antivirus in 'antivirus.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
