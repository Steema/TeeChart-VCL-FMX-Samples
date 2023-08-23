//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TeeMakerC5.res");
USEFORMNS("TeeMakerEditor.pas", Teemakereditor, MakerEditor);
USEFORMNS("TeeMakerMain.pas", Teemakermain, MakerEditor1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->Title = "TeeMaker 1.0";
                 Application->CreateForm(__classid(TMakerEditor1), &MakerEditor1);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
