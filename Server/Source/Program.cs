//Сервер Tcp для ZX AY232K
using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Runtime.InteropServices;

public class SimpleServer
{
        //static string remoteAddress; // хост для отправки данных
        //static int remotePort; // порт для отправки данных
        static byte type_pack; //Тип ответного пакета
        static int pack_size_com; //размер основной части пакета
        static int pack_size_full; //полный размер пакета 
        static int pack_size1024 = 1024 + 32 + 2; //Размер пакета 1024
        static int pack_size256 = 256 + 32 + 2; //Размер пакета 256
        static int pack_size32 = 32 + 2; //Размер пакета 32
        static int files_pag = 64; //Файлов в одной странице каталога
        static int max_file_size = 256*256*1024; //макс размер файла
        static int buf_size = 2000; //размер буфера

    private TcpListener listener;
    private List<TcpClient> clients = new List<TcpClient>(); // Отслеживаются подключенные клиенты

    public void Start(int port) //запуск сервера
    {
        listener = new TcpListener(IPAddress.Any, port);
        listener.Start();
        Console.WriteLine("Server started, waiting for connections...");

        Thread acceptThread = new Thread(AcceptClients);
        acceptThread.Start();
    }

    private void AcceptClients() //приём клиента
    {
        while (true)
        {
            try
            {
                TcpClient client = listener.AcceptTcpClient();
                clients.Add(client);
                Console.WriteLine("Client connected.");

                Thread clientThread = new Thread(() => HandleClient(client));
                clientThread.Start();
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error accepting client: " + ex.Message);
            }
        }
    }

    private void HandleClient(TcpClient client) //обработка клиента
    {
        NetworkStream stream = client.GetStream();
        byte[] data = new byte[buf_size];
        int bytesRead;

        try
        {
            while ((bytesRead = stream.Read(data, 0, data.Length)) > 0)
            { //тут основной код обработки запроса
                
                    IPEndPoint ep = client.Client.RemoteEndPoint as IPEndPoint; //узнать адрес
        
                    Console.WriteLine("Client: " + ep.Address.ToString());

                    if (bytesRead >= pack_size32 && bytesRead <=(pack_size1024)) //если размер пакета подходящий
                    {
                        //Thread.Sleep(5);
                        //Проверка пакета
                        string sym00 = data[0].ToString();
                        string sym01 = data[1].ToString();
                        string sym02 = data[2].ToString();
                        string sym03 = data[3].ToString();
                        if (sym00 == "90" && sym01 == "88" && sym02 == "78") // буквы ZXN
                        {


                            if (sym03 == "0") //если запрос каталога 1024
                            {
                                Console.WriteLine("Incoming packet: 0: Request a catalog");
                                //заполнить каталог
                                string path = Directory.GetCurrentDirectory(); //узнать текущий путь
                                string[] files_in_dir = Directory.GetFiles(path, "*.*"); //получить список файлов в каталоге
                                //if (files_in_dir.Length > files_pag) Array.Resize(ref files_in_dir, files_pag);//ограничение файлов в каталоге
                                //Array.Sort(files_in_dir); //сортировка по имени
                                byte[] data_out = new byte[buf_size]; //буфер для отправки
                                data_out[0] = Convert.ToByte('Z'); //метка заголовка
                                data_out[1] = Convert.ToByte('X');
                                data_out[2] = Convert.ToByte('N');
                                data_out[3] = 1; //тип пакета каталог
                                data_out[4] = data[4]; //смещение каталога (номер страницы) скопируем из запроса
                                data_out[5] = data[5];

                                int file_shift = Int32.Parse(data[4].ToString()) + Int32.Parse(data[5].ToString()) * 256;//прочитаем смещение
                                string long_name = "";
                                for (int i = 0; i < files_pag; i++) //заполним каталог
                                {
                                    if ((i + file_shift * files_pag) < files_in_dir.Length) //если не за пределами массива
                                    {
                                        long_name = files_in_dir[i + file_shift * files_pag]; //длинное имя
                                        string short_name = ShortFileName(long_name); //получить короткий путь
                                        string file_name = Path.GetFileName(short_name);  //получить короткое имя файла
                                        byte[] file_name2 = System.Text.Encoding.UTF8.GetBytes(file_name); //взять имя файла в массив
                                        Array.Copy(file_name2, 0, data_out, i * 16 + 32, file_name2.Length); //копировать часть массива в выходной буфер
                                        //получить размер файла
                                        FileInfo info = new FileInfo(short_name);
                                        long file_size = info.Length;
                                        //записать в каталог 4 байта размера
                                        byte[] file_size_tmp = new byte[4]; //страховка чтобы получить всегда 4 байта длины
                                        file_size_tmp = BitConverter.GetBytes(file_size);
                                        //data_out[i * 16 + 32 + 12] = file_size_tmp[0];//запишем в пакет размер
                                        //data_out[i * 16 + 32 + 13] = file_size_tmp[1];
                                        //data_out[i * 16 + 32 + 14] = file_size_tmp[2];
                                        //data_out[i * 16 + 32 + 15] = file_size_tmp[3];
                                        Array.Copy(file_size_tmp, 0, data_out, i * 16 + 32 + 12, 4); //копировать размер
                                    }
                                }
                                //Посчитаем контрольную сумму
                                int crc = 0;
                                for (int i = 0; i < 1024 + 32; i++) //пакет каталога 1024 байт
                                {
                                    crc = crc + data_out[i];
                                }
                                if (crc > 65535)
                                    crc = crc % 65536; //остаток от деления, переводим в 16 бит число
                                int crc_h = crc / 256;
                                int crc_l = crc - crc_h * 256;
                                data_out[1024 + 32] = Convert.ToByte(crc_l);//запишем в пакет контрольную сумму
                                data_out[1024 + 33] = Convert.ToByte(crc_h);
                                //
                                Console.WriteLine("Outgoing packet 1: Catalog. Part " + file_shift);
                                stream.Write(data_out, 0, pack_size1024); // отправка каталога
                                 

                            }




                            if (sym03 == "2" || sym03 == "6") //если запрос на передачу файла с сервера, 1024 или 256
                            {

                                if (sym03 == "2") {pack_size_full = pack_size1024; pack_size_com = 1024; type_pack = 3;}; //параметры пакета
                                if (sym03 == "6") { pack_size_full = pack_size256; pack_size_com = 256; type_pack = 7; }
                                //Отправка файла
                                Console.WriteLine("Incoming packet: " + sym03 + ": Request to receive file");
                                //прочитаем имя файла из запроса
                                byte[] file_name2 = new byte[12]; //временный массив
                                Array.Copy(data, 16, file_name2, 0, 12); //копировать имя файла
                                string file_in = System.Text.Encoding.UTF8.GetString(file_name2);
                                string[] file_name3 = file_in.Split('\0');
                                file_in = file_name3[0]; // обрежем ноли в конце


                                byte[] data_file = new byte[max_file_size+1024]; //буфер для файла
                                byte[] data_out = new byte[buf_size]; //буфер для отправки
                                data_out[0] = Convert.ToByte('Z'); //метка заголовка
                                data_out[1] = Convert.ToByte('X');
                                data_out[2] = Convert.ToByte('N');
                                Array.Copy(data, 16, data_out, 16, 12); //копировать имя файла

                                try
                                {
                                    FileStream FS_In = new FileStream(file_in, FileMode.Open); //открываем входной файл
                                    int file_in_lenght = Convert.ToInt32(FS_In.Length); //длина файла
                                    if (file_in_lenght > max_file_size) file_in_lenght = max_file_size; //ограничение размера
                                    FS_In.Read(data_file, 0, file_in_lenght); //прочитаем
                                    FS_In.Close();
                                }

                                catch (Exception ex)
                                {
                                    Console.WriteLine(ex.Message);
                                    type_pack = 255; //тип пакета Ошибка (если нет файла или не могли прочитать)
                                }
                                data_out[3] = type_pack; //записать тип пакета для отправки

                                //отправка нужной части
                                int file_shift = Int32.Parse(data[4].ToString()) + Int32.Parse(data[5].ToString()) * 256;//прочитаем смещение
                                Array.Copy(data_file, file_shift * pack_size_com, data_out, 32, pack_size_com); //копировать часть массива в выходной буфер

                                //запишем в заголовок смещение
                                data_out[4] = data[4];
                                data_out[5] = data[5];

                                //Посчитаем контрольную сумму
                                int crc = 0;
                                for (int i = 0; i < pack_size_com + 32; i++) //
                                {
                                    crc = crc + data_out[i];
                                }
                                if (crc > 65535)
                                    crc = crc % 65536; //остаток от деления, переводим в 16 бит число
                                int crc_h = crc / 256;
                                int crc_l = crc - crc_h * 256;
                                data_out[pack_size_com + 32] = Convert.ToByte(crc_l);//запишем в пакет контрольную сумму
                                data_out[pack_size_com + 33] = Convert.ToByte(crc_h);
                                //
                                if (type_pack == Convert.ToByte("255"))
                                    Console.WriteLine("Outgoing packet "+type_pack+": Error");
                                else Console.WriteLine("Outgoing packet " + type_pack + ": File " + file_in + " part " + file_shift);

                                stream.Write(data_out, 0, pack_size_full); // отправка части
                            }



                            if (sym03 == "4" || sym03 == "8") //если запрос на передачу файла на сервер, 1024 или 256
                            {
                                //Приём файла
                                Console.WriteLine("Incoming packet: " + sym03 + ": File Transfer Request");
                                if (sym03 == "4") { pack_size_full = pack_size32; pack_size_com = 1024; type_pack = 5; }; //параметры пакета
                                if (sym03 == "8") { pack_size_full = pack_size32; pack_size_com = 256; type_pack = 9; }

                                byte[] data_file = new byte[max_file_size+1024]; //буфер для файла
                                byte[] data_out = new byte[buf_size]; //буфер для отправки

                                //Посчитаем контрольную сумму принятого пакета
                                int crc = 0;
                                for (int i = 0; i < pack_size_com + 32; i++) //
                                {
                                    crc = crc + data[i];
                                }
                                if (crc > 65535)
                                    crc = crc % 65536; //остаток от деления, переводим в 16 бит число
                                int crc_h = crc / 256;
                                int crc_l = crc - crc_h * 256;
                                if (data[pack_size_com + 32] != Convert.ToByte(crc_l) || data[pack_size_com + 33] != Convert.ToByte(crc_h))
                                    type_pack = 255; //Ошибка Если не равна

                                //прочитаем имя файла из запроса
                                byte[] file_name2 = new byte[12]; //временный массив
                                Array.Copy(data, 16, file_name2, 0, 12); //копировать имя файла
                                string file_out = System.Text.Encoding.UTF8.GetString(file_name2);
                                string[] file_name3 = file_out.Split('\0');
                                file_out = file_name3[0]; // обрежем ноли в конце

                                //размер файла из запроса
                                int file_size = Convert.ToInt16(data[12]) + Convert.ToInt16(data[13])*256 + Convert.ToInt16(data[14])*256*256 + Convert.ToInt16(data[15])*256*256*256;

                                //Проверить есть ли такой файл
                                FileStream FS_Out = null;
                                if (File.Exists(file_out))
                                {
                                    try
                                    {
                                        FS_Out = new FileStream(file_out, FileMode.Open); //открываем выходной файл
                                        int file_in_lenght = Convert.ToInt32(FS_Out.Length); //длина файла
                                        if (file_in_lenght > max_file_size) file_in_lenght = max_file_size; //ограничение размера
                                        FS_Out.Read(data_file, 0, file_in_lenght); //прочитаем
                                        FS_Out.Close(); //закрыть
                                    }
                                    catch (Exception ex)
                                    {
                                        Console.WriteLine(ex.Message);
                                        type_pack = 255; //тип пакета Ошибка (если нет файла или не могли прочитать)
                                    }
                                }
                                else
                                {
                                    try
                                    {
                                        FS_Out = new FileStream(file_out, FileMode.Create);  //или создаём выходной файл
                                        FS_Out.Close(); //закрыть
                                    }
                                    catch (Exception ex)
                                    {
                                        Console.WriteLine(ex.Message);
                                        type_pack = 255; //тип пакета Ошибка, если не могли создать
                                    }
                                }

                                //подготовка нужной части
                                int file_shift = Int32.Parse(data[4].ToString()) + Int32.Parse(data[5].ToString()) * 256;//прочитаем смещение
                                Array.Copy(data, 32, data_file, file_shift * pack_size_com, pack_size_com); //копировать часть массива в файловый буфер

                                if (type_pack != Convert.ToByte("255")) //запишем в файл если пакет хороший
                                {
                                    try
                                    {
                                        FS_Out = new FileStream(file_out, FileMode.Open); //открываем выходной файл
                                        FS_Out.Write(data_file, 0, file_size); //запишем
                                        FS_Out.Close();
                                    }
                                    catch (Exception ex)
                                    {
                                        Console.WriteLine(ex.Message);
                                        type_pack = 255; //тип пакета Ошибка, если не могли создать
                                    }
                                }
                                //Подготовим ответ
                                data_out[0] = Convert.ToByte('Z'); //метка заголовка
                                data_out[1] = Convert.ToByte('X');
                                data_out[2] = Convert.ToByte('N');
                                data_out[3] = type_pack; //тип пакета Файл принят или Ошибка
                                Array.Copy(data, 16, data_out, 16, 12); //копировать имя файла

                                //запишем в заголовок смещение
                                data_out[4] = data[4];
                                data_out[5] = data[5];

                                //Посчитаем контрольную сумму
                                crc = 0;
                                for (int i = 0; i < 32; i++) //ответный пакет 32
                                {
                                    crc = crc + data_out[i];
                                }
                                if (crc > 65535)
                                    crc = crc % 65536; //остаток от деления, переводим в 16 бит число
                                crc_h = crc / 256;
                                crc_l = crc - crc_h * 256;
                                data_out[32] = Convert.ToByte(crc_l);//запишем в пакет контрольную сумму
                                data_out[33] = Convert.ToByte(crc_h);
                                //

                                if (type_pack == Convert.ToByte("255"))
                                    Console.WriteLine("Outgoing packet "+type_pack+": Error");
                                else
                                    Console.WriteLine("Outgoing packet "+type_pack+": File accepted " + file_out + " part " + file_shift);

                                stream.Write(data_out, 0, pack_size_full); // отправка подтверждения
                            }


 
                            
                        }
                        else
                            Console.WriteLine("Incoming packet: Unknown, length "+bytesRead);

                    }

            }
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error communicating with client: " + ex.Message);
        }
        finally
        {
            stream.Close();
            client.Close();
            clients.Remove(client);
            Console.WriteLine("Client disconnected.");
        }

        
    }

        // Определить функцию API GetShortPathName.
        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        static extern uint GetShortPathName(string lpszLongPath,
        char[] lpszShortPath, int cchBuffer);

        // Возвращает короткое имя файла для длинного имени файла.
        private static string ShortFileName(string long_name)
        {
            char[] name_chars = new char[1024];
            long length = GetShortPathName(
                long_name, name_chars,
                name_chars.Length);

            string short_name = new string(name_chars);
            return short_name.Substring(0, (int)length);
        }

        // Возвращает длинное имя файла для короткого имени файла.
        private static string LongFileName(string short_name)
        {
            return new FileInfo(short_name).FullName;
        }

}

class Program
{
    static string ver = "2025 03 20"; //версия сервера
    static int localPort; // локальный порт для прослушивания входящих подключений
    static void Main(string[] args) //старт приложения тут
    {
        try
            {
                if (args.Length == 0) // если не передан параметр
                    localPort = 8888; //порт будет такой
                else
                    localPort = Int32.Parse(args[0]); //или получим номер порта как параметр
                Console.Write("ZXNServerK v." + ver + "\n");
                Console.Write("Listening port: " + localPort + "\n"); // локальный порт

                SimpleServer server = new SimpleServer();
                server.Start(localPort); // Выбирается порт, любой


            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }

        
    }

}

