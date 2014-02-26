/*
 * A Java RMI server for spell checking. 
 * Spring 2014 CSCI182E 
 * Tum Chaturapruek
 */

package words;

import java.io.IOException;
import java.rmi.AlreadyBoundException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

public final class RMIServer implements RemoteSpelling{
  /**
   * The actual remote spell check being exported (to avoid garbage
   * collection).
   */
  private static RemoteSpelling static_spelling;
  /**
   * The minimum acceptable port number.
   */
  private static final int MIN_PORT_NUMBER = 1025;

  /**
   * The maximum port number.
   */
  public static final int MAX_PORT_NUMBER = 65535;

  /**
   * the time stamp format.
   */
  public static final SimpleDateFormat TIME_STAMP_FORMAT =
      new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
  
  private static WordList word_list;
  /**
   * Private constructor (to prevent instantiation).
   */
  private RMIServer() throws IOException
  {
    // Do nothing
  }
  
  public static String getDateTime()
  {
    return TIME_STAMP_FORMAT.format(new Date(System.currentTimeMillis()));
  }
  
  public static void println(String the_string)
  {
    System.out.println(RMIServer.getDateTime() + ": " + the_string);
  }
  
  public static void print(String the_string)
  {
    System.out.print(RMIServer.getDateTime() + ": " + the_string);
  }
  
  public static void println_err(String the_string)
  {
    System.err.println(RMIServer.getDateTime() + ": " + the_string);
  }
  /**
   * The main method (there is nothing else to this class): sends
   * a query line to the host (first argument) and port (second
   * argument) containing the specified String (third argument).
   * @param the_args The command line arguments.
   * @throws NotBoundException 
   * @throws IOException 
   * @throws AlreadyBoundException 
   */
  public static void main(final String[] the_args)
  {
    try
    {
      if (the_args.length != 3)
      {
        throw new ArrayIndexOutOfBoundsException();
      }
      final int port = Integer.parseInt(the_args[0]);
      final String service_name = the_args[1];
      final String dictionary_path = the_args[2];
      
      if (port < MIN_PORT_NUMBER || port > MAX_PORT_NUMBER)
      {
        RMIServer.println_err("port number must be between " +
            MIN_PORT_NUMBER + " and " +
            MAX_PORT_NUMBER + " inclusive");
        System.exit(1);
      }
      word_list = new WordList(dictionary_path);
      final Registry registry = LocateRegistry.createRegistry(port);
      static_spelling = new RMIServer();
      registry.bind(service_name,
          UnicastRemoteObject.exportObject(static_spelling, 0));
      RMIServer.println("Exported spelling service as " + 
          service_name +" on registry port " + port);
    }
    catch (final RemoteException e)
    {
      RMIServer.println_err("A problem occurred: " + e);
      System.exit(1);
    }
    catch (final AlreadyBoundException e)
    {
      RMIServer.println_err("Already bound???: " + e);
      System.exit(1);
    }
    catch (IOException e)
    {
      RMIServer.println_err("IO exception: " + e);
      System.exit(1);
    }
    catch (final NumberFormatException e)
    {
      RMIServer.println_err("destination port number is invalid");
      System.exit(1);
    }
    catch (final ArrayIndexOutOfBoundsException e)
    {
      RMIServer.println_err("Usage: java words.RMIServer <port> "
          + "<service-name> <word-list>");
      System.exit(1);
    }
  }

  @Override
  public List<String> check(String the_word) throws RemoteException {
    RMIServer.println("Received query (" + the_word + ")");
    if (word_list.correct(the_word))
    {
      // the word is spelled correctly.
      RMIServer.println(the_word + " is spelled correctly.");
      return null;
    }
    else
    {
      // the word is spelled incorrectly.
      List<String> suggestions = word_list.suggestions(the_word);
      final int num_suggestions = suggestions.size();
      String suggestion_context;
      if (num_suggestions == 1)
      {
        suggestion_context = " There is " + num_suggestions +
            " suggested word:";
      }
      else
      {
        suggestion_context = " There are " + num_suggestions +
            " suggested words:";
      }
      RMIServer.print(the_word +" is spelled incorrectly." +
        suggestion_context);
      for (String suggestion : suggestions)
      {
        System.out.print(" " + suggestion);
      }
      System.out.println(".");
      return suggestions;
    }
  }

  @Override
  public void add(String the_word) throws RemoteException {
    if (word_list.add(the_word))
    {
      RMIServer.println(the_word + " added to word list.");
    }
    else
    {
      RMIServer.println(the_word + " already in word list, not added.");
    }
  }

  @Override
  public void remove(String the_word) throws RemoteException {
    if (word_list.remove(the_word))
    {
      RMIServer.println(the_word + " from word list.");
    }
    else
    {
      RMIServer.println(the_word + " not in word list, not removed.");
    }
  }
}
