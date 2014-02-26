/*
 * A Java RMI client for spell checking. 
 * Spring 2014 CSCI182E 
 * Tum Chaturapruek
 */

package words;

import java.io.IOException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;


public final class RMIClient{
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
  
  /**
   * Private constructor (to prevent instantiation).
   */
  private RMIClient() throws IOException
  {
    // Do nothing
  }
  
  public static String getDateTime()
  {
    return TIME_STAMP_FORMAT.format(new Date(System.currentTimeMillis()));
  }
  
  public static void println(String the_string)
  {
    System.out.println(RMIClient.getDateTime() + ": " + the_string);
  }
  
  public static void print(String the_string)
  {
    System.out.print(RMIClient.getDateTime() + ": " + the_string);
  }
  
  public static void println_err(String the_string)
  {
    System.err.println(RMIClient.getDateTime() + ": " + the_string);
  }
  /**
   * The main method (there is nothing else to this class): sends
   * a query line to the host (first argument) and port (second
   * argument) containing the specified String (third argument).
   * @param the_args The command line arguments.
   */
  public static void main(final String[] the_args)
  {
    try
    {
      final String host = the_args[0];
      final int port = Integer.parseInt(the_args[1]);
      final String service_name = the_args[2];
      
      if (port < MIN_PORT_NUMBER || port > MAX_PORT_NUMBER)
      {
        RMIClient.println_err("port number must be between " +
            MIN_PORT_NUMBER + " and " +
            MAX_PORT_NUMBER + " inclusive");
        System.exit(1);
      }

      final Registry registry = LocateRegistry.getRegistry(host, port);
      // Look up in the registry
      final RemoteSpelling remote_spelling =
          (RemoteSpelling) registry.lookup(service_name);
      
      for (int i = 3; i < the_args.length; i++)
      {
        
        final String the_ith_string = the_args[i];
        RMIClient.println("Querying service for " + the_ith_string);
        final List<String> suggestions = remote_spelling.check(the_ith_string);
        
        if (suggestions == null)
        {
          // the spelling is correct.
          RMIClient.println(the_ith_string + " is spelled correctly.");
        }
        else
        {
          // the spelling is incorrect
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
          RMIClient.print(the_ith_string +" is spelled incorrectly." +
            suggestion_context);
          for (String suggestion : suggestions)
          {
            System.out.print(" " + suggestion);
          }
          System.out.println(".");
        }       
      }
    }
    catch (final RemoteException e)
    {
      RMIClient.println_err("A problem occurred: " + e);
      System.exit(1);
    }
    catch (final NotBoundException e)
    {
      RMIClient.println_err("Could not find RMI server.");
      System.exit(1);
    }
    catch (final NumberFormatException e)
    {
      RMIClient.println_err("destination port number is invalid");
      System.exit(1);
    }
    catch (final ArrayIndexOutOfBoundsException e)
    {
      RMIClient.println_err("Usage: java words.RMIClient <host> <port>"
          + " <service-name> <query-word> [query_word] ...");
      System.exit(1);
    }
  }
}
