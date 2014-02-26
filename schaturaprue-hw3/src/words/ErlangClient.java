/*
 * A Java Erlang client for spell checking. 
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

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

public final class ErlangClient implements RemoteSpelling{
  /**
   * The minimum number of command line arguments.
   */
  private static final int MIN_ARGS = 2;

  /**
   * The timeout period, in milliseconds.
   */
  private static final int TIMEOUT = 10000;

  
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
  private ErlangClient() throws IOException
  {
    // Do nothing
  }
  
  public static String getDateTime()
  {
    return TIME_STAMP_FORMAT.format(new Date(System.currentTimeMillis()));
  }
  
  public static void println(String the_string)
  {
    System.out.println(ErlangClient.getDateTime() + ": " + the_string);
  }
  
  public static void print(String the_string)
  {
    System.out.print(ErlangClient.getDateTime() + ": " + the_string);
  }
  
  public static void println_err(String the_string)
  {
    System.err.println(ErlangClient.getDateTime() + ": " + the_string);
  }
  /**
   * The main method (there is nothing else to this class): sends
   * a query line to the host (first argument) and port (second
   * argument) containing the specified String (third argument).
   * @param the_args The command line arguments.
   *   The first is the Erlang node name of the server;
   *   the remainder are query words.
   */
  public static void main(final String[] the_args)
  {
    try
    {
      if (the_args.length < MIN_ARGS)
      {
        throw new ArrayIndexOutOfBoundsException();
      }

      final String node_name = the_args[0];
      final OtpSelf me = new OtpSelf("schaturaprue" + System.currentTimeMillis());
      final OtpConnection connection = me.connect(new OtpPeer(node_name));

      System.out.print("Sending message to " + the_args[0] + " at " + the_args[1] + ": ");
      for (int i = 2; i < the_args.length; i++)
      {
        System.out.print(the_args[i] + " ");
      }
      System.out.println();
      
      final OtpErlangObject[] message_strings = new OtpErlangObject[the_args.length - 2];
      for (int i = 2; i < the_args.length; i++)
      {
        message_strings[i-2] = new OtpErlangString(the_args[i]);
      }
      final OtpErlangList message = new OtpErlangList(message_strings);
      final OtpErlangRef ref = me.createRef();
      final OtpErlangTuple tuple =
        new OtpErlangTuple(new OtpErlangObject[] { me.pid(), ref, message });
      connection.send(the_args[0], tuple);
      final OtpErlangObject reply = connection.receive(TIMEOUT);

      if (reply instanceof OtpErlangTuple &&
          ((OtpErlangTuple) reply).arity() == 3)
      {
        try
        {
          final OtpErlangTuple reply_tuple = (OtpErlangTuple) reply;
          final OtpErlangRef reply_ref = (OtpErlangRef) reply_tuple.elementAt(0);
          final OtpErlangLong reply_long = (OtpErlangLong) reply_tuple.elementAt(1);
          final OtpErlangObject reply_obj = reply_tuple.elementAt(2);
          if (ref.equals(reply_ref)) {
            System.out.println("Reply " + reply_long + " from server: " + reply_obj);
          }
        }
        catch (final ClassCastException e)
        {
          System.out.println("Got unexpected message: " + reply);
        }
      }
    }
    catch (final RemoteException e)
    {
      ErlangClient.println_err("A problem occurred: " + e);
      System.exit(1);
    }
    catch (final NumberFormatException e)
    {
      ErlangClient.println_err("destination port number is invalid");
      System.exit(1);
    }
    catch (final ArrayIndexOutOfBoundsException e)
    {
      ErlangClient.println_err("Usage: java words.ErlangClient"
          + "<Erlang-node-name> <word-list>");
      System.exit(1);
    }
    catch (final IOException | InterruptedException | OtpAuthException | OtpErlangExit e1) {
      ErlangClient.println_err("An error occurred.");
      e1.printStackTrace();
      System.exit(1);
    }
  }

  @Override
  public List<String> check(String the_word) throws RemoteException {
    ErlangClient.println("Received query (" + the_word + ")");
    if (word_list.correct(the_word))
    {
      // the word is spelled correctly.
      ErlangClient.println(the_word + " is spelled correctly.");
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
      ErlangClient.print(the_word +" is spelled incorrectly." +
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
      ErlangClient.println(the_word + " added to word list.");
    }
    else
    {
      ErlangClient.println(the_word + " already in word list, not added.");
    }
  }

  @Override
  public void remove(String the_word) throws RemoteException {
    if (word_list.remove(the_word))
    {
      ErlangClient.println(the_word + " from word list.");
    }
    else
    {
      ErlangClient.println(the_word + " not in word list, not removed.");
    }
  }
}
