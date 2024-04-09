{********************************************}
{ TeeMaker 2.0                               }
{ Copyright (c) 2002-2024 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeMakerConst;
{$I TeeDefs.inc}

interface

type
  TMakerLanguages=class
  public
    class procedure English;
    class procedure Spanish;
  end;

const
  BlockAction_LeftClick        = '0';
  BlockAction_RightClick       = '1';
  BlockAction_LeftDrag         = '2';
  BlockAction_MouseEnter       = '3';
  BlockAction_MouseExit        = '4';
  BlockAction_RightDrag        = '5';
  BlockAction_LeftDoubleClick  = '6';
  BlockAction_RightDoubleClick = '7';
  BlockAction_WheelDrag        = '8';
  BlockAction_LifeBirth        = '9';
  BlockAction_LifeDeath        = '10';

var
  // Constants
  TeeMakerVersion:String='TeeMaker 2.0.7' {$IFDEF TEEBETA}+' Beta'{$ENDIF};
  TeeMakerExtension:String='.hou';
  TeeMakerLibRegistry:String='LibraryPath';
  TeeMsg_WebLibraryURL:String='http://www.teechart.net/files/beta/teeMaker/library/objects';
  TeeMsg_DefaultFormat:String='0.###';

  // Variables

  TeeMsg_TexturesLibrary:String;
  TeeMsg_ObjectsLibrary:String;
  TeeMsg_SoundsLibrary:String;

  TeeMsg_CannotFind:String;
  TeeMsg_ErrorCannotLink:String;
  TeeMsg_ErrorTesselator:String;
  TeeMsg_OpenWithTeeMaker:String;
  TeeMsg_TeeMakerFile:String;
  TeeMsg_SureNewFile:String;
  TeeMsg_TeeMakerWelcome:String;
  TeeMsg_MakerLibraryFolder:String;
  TeeMsg_MakerLibraryObjects:String;
  TeeMsg_MakerLibrarySounds:String;
  TeeMsg_MakerLibraryTextures:String;
  TeeMsg_TeeMakerFiles:String;
  TeeMsg_SureToDelete:String;
  TeeMsg_Untitled:String;
  TeeMsg_TeeMaker:String;
  TeeMsg_MakerLoaded:String;
  TeeMsg_CannotFindFile:String;
  TeeMsg_FileReadOnly:String;
  TeeMsg_SaveChanges:String;
  TeeMsg_CopyOfBlock:String;
  TeeMsg_LinkToBlock:String;
  TeeMsg_Folder:String;
  TeeMsg_SelectLibraryFolder:String;
  TeeMsg_ObjectBlock:String;
  TeeMsg_CannotExecute:String;
  TeeMsg_ColorReplacer:String;
  TeeMsg_Animation:String;
  TeeMsg_Animations:String;
  TeeMsg_NewAnimation:String;
  TeeMsg_RenameAnimation:String;
  TeeMsg_RotateBlockHelp:String;
  TeeMsg_MoveBlockHelp:String;
  TeeMsg_SizeBlockHelp:String;
  TeeMsg_SureToReload:String;
  TeeMsg_SureToConvertLink:String;
  TeeMsg_MakerBasic:String;
  TeeMsg_WebLibrary:String;
  TeeMsg_FolderToLink:String;
  TeeMsg_Link:String;
  TeeMsg_ErrorRenamingFolder:String;
  TeeMsg_ClearCaption:String;
  TeeMsg_LoadCaption:String;
  TeeMsg_CachedFonts:String;
  TeeMsg_MakerWorld:String;
  TeeMsg_MakerRender:String;
  TeeMsg_MakerBlocks:String;
  TeeMsg_CreateFolder:String;
  TeeMsg_CannotSaveFile:String;
  TeeMsg_SelfBlock:String;
  TeeMsg_SystemObject:String;

  Tee_MouseLeft:String;
  Tee_MouseRight:String;
  Tee_MouseLeftDrag:String;
  Tee_MouseRightDrag:String;
  Tee_MouseEnter:String;
  Tee_MouseExit:String;
  Tee_MouseLeftDoubleClick:String;
  Tee_MouseRightDoubleClick:String;
  Tee_MouseWheelDrag:String;
  Tee_LifeBirth:String;
  Tee_LifeDeath:String;

  TeeMsg_SureToDeleteAction:String;
  TeeMsg_SureToDeleteProperty:String;
  TeeMsg_PropertyCannotBeEmpty:String;
  TeeMsg_PropertyAlreadyExists:String;
  TeeMsg_NewProperty:String;
  TeeMsg_Property:String;
  TeeMsg_ChangeProperty:String;

implementation

class procedure TMakerLanguages.English;
begin
  TeeMsg_TexturesLibrary:='Textures';
  TeeMsg_ObjectsLibrary:='Objects';
  TeeMsg_SoundsLibrary:='Sounds';

  TeeMsg_CannotFind:='The system cannot find the file specified.';
  TeeMsg_ErrorCannotLink:='Cannot link block to itself (recursive).';
  TeeMsg_ErrorTesselator:='OpenGL Error creating Tesselator object.';

  TeeMsg_OpenWithTeeMaker:='Open with &TeeMaker';
  TeeMsg_TeeMakerFile:='TeeMaker file';
  TeeMsg_SureNewFile:='Sure to create a new file?';

  TeeMsg_TeeMakerWelcome:= 'Welcome to TeeMaker !'#13+#13+
                    'NOTE: A folder named "Library" will be created under current'#13+
                    'application folder: (%s)'+#13+
                    'to store reusable objects, sounds and images.'#13+#13+
                    'You can always change the location of this Library folder using the menu option: '+#13+#13+
                    'Tools --> Library folder';

  TeeMsg_MakerLibraryFolder:='Library';
  TeeMsg_MakerLibraryObjects:='Objects';
  TeeMsg_MakerLibrarySounds:='Sounds';
  TeeMsg_MakerLibraryTextures:='Textures';

  TeeMsg_TeeMakerFiles:='TeeMaker files (*%s)|*%s';

  TeeMsg_SureToDelete:='Sure to delete: %s?';
  TeeMsg_Untitled:='(untitled)';
  TeeMsg_TeeMaker:='TeeMaker - %s';
  TeeMsg_MakerLoaded:='Loaded in: %s msec.';
  TeeMsg_CannotFindFile:='Cannot find file: '#13+'%s';

  TeeMsg_FileReadOnly:='File: %s is read only.'+#13+
                     'Do you want to force write it?';

  TeeMsg_SaveChanges:='Save changes to %s?';

  TeeMsg_CopyOfBlock:='Copy of %s';
  TeeMsg_LinkToBlock:='Link to %s';
  TeeMsg_Folder:='Folder';

  TeeMsg_SelectLibraryFolder:='Select default library folder';
  TeeMsg_ObjectBlock:='Object';
  TeeMsg_CannotExecute:='Cannot execute: %s';
  TeeMsg_ColorReplacer:='Color replacer';

  TeeMsg_Animation:='Animation %s';
  TeeMsg_Animations:='Animations';
  TeeMsg_NewAnimation:='New animation name';
  TeeMsg_RenameAnimation:='Rename animation';

  TeeMsg_RotateBlockHelp:='Drag mouse to rotate block. Hold CTRL key to tilt.';
  TeeMsg_MoveBlockHelp:='Drag mouse to move block. Hold CTRL key to move vertically.';
  TeeMsg_SizeBlockHelp:='Drag mouse to resize block. Hold CTRL key to resize depth.';

  TeeMsg_SureToReload:='Sure to reload source?';
  TeeMsg_SureToConvertLink:='Sure to convert from link to local copy?';

  TeeMsg_MakerBasic:='Basic';
  TeeMsg_WebLibrary:='Web';

  TeeMsg_FolderToLink:='Select folder to link';
  TeeMsg_Link:='Link';

  TeeMsg_ErrorRenamingFolder:='Error renaming directory from: '+#13
                                     +'%s'+#13+' to: '+#13+'%s'+ #13+#13+'%s';

  TeeMsg_ClearCaption:='&Clear';
  TeeMsg_LoadCaption:='&Load';

  TeeMsg_CachedFonts:='Cached fonts: %s';

  TeeMsg_MakerWorld:='World';
  TeeMsg_MakerRender:='Render';
  TeeMsg_MakerBlocks:='Blocks';
  TeeMsg_SystemObject:='System';

  TeeMsg_CreateFolder:='Folder does not exist.'#13+'Do you want to create it?'+#13+'%s';
  TeeMsg_CannotSaveFile:='Cannot save file: '#13+'%s';

  TeeMsg_SelfBlock:='Current Block';

  Tee_MouseLeft     := 'Mouse.Left.Click';
  Tee_MouseRight    := 'Mouse.Right.Click';
  Tee_MouseLeftDrag := 'Mouse.Left.Drag';
  Tee_MouseRightDrag:= 'Mouse.Right.Drag';
  Tee_MouseEnter    := 'Mouse.Enter';
  Tee_MouseExit     := 'Mouse.Exit';
  Tee_MouseLeftDoubleClick := 'Mouse.Left.Double Click';
  Tee_MouseRightDoubleClick := 'Mouse.Right.Double Click';
  Tee_MouseWheelDrag:=  'Mouse.Wheel.Drag';
  Tee_LifeBirth     := 'Life.Birth';
  Tee_LifeDeath     := 'Life.Death';

  TeeMsg_SureToDeleteAction:='Sure to delete action?';
  TeeMsg_SureToDeleteProperty:='Sure to delete property [%s]?';
  TeeMsg_PropertyCannotBeEmpty:='Property name cannot be empty.';
  TeeMsg_PropertyAlreadyExists:='A Property already exists with this name.';
  TeeMsg_NewProperty:='New Property';
  TeeMsg_Property:='Property';
  TeeMsg_ChangeProperty:='Change Property Name';
end;

class procedure TMakerLanguages.Spanish;
begin
  TeeMsg_TexturesLibrary:='Texturas';
  TeeMsg_ObjectsLibrary:='Objetos';
  TeeMsg_SoundsLibrary:='Sonidos';

  TeeMsg_CannotFind:='No se puede encontrar el archivo especificado.';
  TeeMsg_ErrorCannotLink:='No es posible enlazar un objeto consigo mismo (recursividad).';
  TeeMsg_ErrorTesselator:='Error en OpenGL al crear un objeto "Tesselator".';

  TeeMsg_OpenWithTeeMaker:='Abrir con &TeeMaker';
  TeeMsg_TeeMakerFile:='Archivo TeeMaker';
  TeeMsg_SureNewFile:=string('�Est� seguro de crear un nuevo archivo?');

  TeeMsg_TeeMakerWelcome:= 'Bienvenido a TeeMaker !'#13+#13+
                    'NOTE: Una carpeta de nombre "Libreria" se crear� en la carpeta actual:'+#13+
                    '(%s)'+#13+
                    'para almacenar objetos reutilizables, sonidos e im�genes.'#13+#13+
                    'Es posible cambiar la carpeta "Libreria" usando la opci�n de men�: '+#13+#13+
                    'Herramientas --> Carpeta de Libreria';

  TeeMsg_MakerLibraryFolder:='Libreria';
  TeeMsg_MakerLibraryObjects:='Objetos';
  TeeMsg_MakerLibrarySounds:='Sonidos';
  TeeMsg_MakerLibraryTextures:='Texturas';

  TeeMsg_TeeMakerFiles:='Archivos TeeMaker (*%s)|*%s';

  TeeMsg_SureToDelete:=string('�Est� seguro de borrar: %s?');
  TeeMsg_Untitled:='(sin nombre)';
  TeeMsg_TeeMaker:='TeeMaker - %s';
  TeeMsg_MakerLoaded:='Cargado en: %s msec.';
  TeeMsg_CannotFindFile:='No es posible encontrar el archivo: '#13+'%s';

  TeeMsg_FileReadOnly:='El archivo: %s es de s�lo lectura.'+#13+
                     '�Desea reescribirlo?';

  TeeMsg_SaveChanges:='�Grabar los cambios efectuados en %s?';

  TeeMsg_CopyOfBlock:='Copia de %s';
  TeeMsg_LinkToBlock:='Enlace a %s';
  TeeMsg_Folder:='Carpeta';

  TeeMsg_SelectLibraryFolder:='Seleccione la carpeta de Libreria por defecto';
  TeeMsg_ObjectBlock:='Objeto';
  TeeMsg_CannotExecute:='No es posible ejecutar la aplicaci�n: %s';
  TeeMsg_ColorReplacer:='Reemplazar Color';

  TeeMsg_Animation:='Animaci�n %s';
  TeeMsg_Animations:='Animaciones';
  TeeMsg_NewAnimation:='Nombre de nueva animaci�n';
  TeeMsg_RenameAnimation:='Renombrar animaci�n';

  TeeMsg_RotateBlockHelp:='Arrastrar el rat�n para rotar el objeto. Presione la tecla CTRL para inclinarlo.';
  TeeMsg_MoveBlockHelp:='Arrastrar el rat�n para mover el objeto. Presione la tecla CTRL para moverlo verticalmente.';
  TeeMsg_SizeBlockHelp:='Arrastrar el rat�n para redimensionar el objeto. Presione la tecla CTRL para cambiar la profundidad.';

  TeeMsg_SureToReload:=string('�Est� seguro de recargar el texto fuente?');
  TeeMsg_SureToConvertLink:=string('�Est� seguro de convertir el enlace en una copia local?');

  TeeMsg_MakerBasic:='Basicos';
  TeeMsg_WebLibrary:='Web';

  TeeMsg_FolderToLink:='Seleccione carpeta a enlazar';
  TeeMsg_Link:='Enlace';

  TeeMsg_ErrorRenamingFolder:='Error renombrando la carpeta: '+#13
                                     +'%s'+#13+' a: '+#13+'%s'+ #13+#13+'%s';

  TeeMsg_ClearCaption:='&Eliminar';
  TeeMsg_LoadCaption:='&Cargar';

  TeeMsg_CachedFonts:='Fuentes en memoria: %s';

  TeeMsg_MakerWorld:='Mundo';
  TeeMsg_MakerRender:='Render';
  TeeMsg_MakerBlocks:='Objetos';

  TeeMsg_CreateFolder:='La carpeta no existe.'#13+'�Desea crearla?'+#13+'%s';
  TeeMsg_CannotSaveFile:='No es posible grabar el archivo: '#13+'%s';

  TeeMsg_SelfBlock:='Objeto Actual';

  Tee_MouseLeft     := 'Rat�n.Bot�n izquierdo.Clic';
  Tee_MouseRight    := 'Rat�n.Bot�n derecho.Clic';
  Tee_MouseLeftDrag := 'Rat�n.Bot�n izquierdo.Arrastrar';
  Tee_MouseRightDrag:= 'Rat�n.Bot�n derecho.Arrastrar';
  Tee_MouseEnter    := 'Rat�n.Entrada';
  Tee_MouseExit     := 'Rat�n.Salida';
  Tee_MouseLeftDoubleClick := 'Rat�n.Bot�n izquierdo.Doble Clic';
  Tee_MouseRightDoubleClick := 'Rat�n.Bot�n derecho.Doble Clic';
  Tee_MouseWheelDrag:=  'Rat�n.Rueda.Arrastrar';

  TeeMsg_SureToDeleteAction:='�Est�s seguro de eliminar la acci�n?';
  TeeMsg_SureToDeleteProperty:='�Est�s seguro de eliminar la propiedad [%s]?';
  TeeMsg_PropertyCannotBeEmpty:='La propiedad no puede tener un nombre vac�o.';
  TeeMsg_PropertyAlreadyExists:='La propiedad ya existe con �ste nombre.';
  TeeMsg_NewProperty:='Nueva Propiedad';
  TeeMsg_Property:='Propiedad';
  TeeMsg_ChangeProperty:='Cambiar el nombre de la Propiedad';
end;

initialization
  TMakerLanguages.English;
end.

