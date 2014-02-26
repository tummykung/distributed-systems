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
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

public final class ErlangClient
{
  /**
   * The minimum number of command line arguments.
   */
  private static final int MIN_ARGS = 2;

  /**
   * The timeout period, in milliseconds.
   */
  private static final int TIMEOUT = 10000;

  
  /**
   * The atoms in the protocol.
   */
  private static final OtpErlangAtom check = new OtpErlangAtom("check");
  private static final OtpErlangAtom add = new OtpErlangAtom("add");
  private static final OtpErlangAtom remove = new OtpErlangAtom("remove");
  private static final OtpErlangAtom correct = new OtpErlangAtom("correct");
  private static final OtpErlangAtom incorrect = new OtpErlangAtom("incorrect");

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
      final String process_name = "spelling";
      final OtpSelf me = new OtpSelf("schaturaprue" + System.currentTimeMillis());
      final OtpConnection connection = me.connect(new OtpPeer(node_name));
      
      for (int i = 1; i < the_args.length; i++)
      {
        final OtpErlangString message = new OtpErlangString(the_args[i]);
        ErlangClient.println("Querying server (" + message + ")");
        final OtpErlangRef ref = me.createRef();
        final OtpErlangTuple tuple =
          new OtpErlangTuple(new OtpErlangObject[] { me.pid(), ref, check, message });
        connection.send(process_name, tuple);
        final OtpErlangObject reply = connection.receive(TIMEOUT);

        if (reply instanceof OtpErlangTuple &&
            ((OtpErlangTuple) reply).arity() == 3)
        {
          try
          {
            final OtpErlangTuple reply_tuple = (OtpErlangTuple) reply;
            final OtpErlangRef reply_ref = (OtpErlangRef) reply_tuple.elementAt(0);
            final OtpErlangAtom reply_result = (OtpErlangAtom) reply_tuple.elementAt(1);
            final OtpErlangList reply_suggestions = (OtpErlangList) reply_tuple.elementAt(2);
            if (ref.equals(reply_ref) && reply_result.equals(incorrect))
            {
              ErlangClient.println(message + " is spelled incorrectly. Suggestions: " + reply_suggestions);
            }
            else
            {
              throw new ClassCastException();
            }
          }
          catch (final ClassCastException e)
          {
            ErlangClient.println_err("Got unexpected message: " + reply);
          }
        }
        else if (reply instanceof OtpErlangTuple &&
            ((OtpErlangTuple) reply).arity() == 2)
        {
          try
          {
            final OtpErlangTuple reply_tuple = (OtpErlangTuple) reply;
            final OtpErlangRef reply_ref = (OtpErlangRef) reply_tuple.elementAt(0);
            final OtpErlangAtom reply_result = (OtpErlangAtom) reply_tuple.elementAt(1);
            if (ref.equals(reply_ref) && reply_result.equals(correct))
            {
              ErlangClient.println(message + " is spelled correctly.");
            }
            else if (ref.equals(reply_ref) && reply_result.equals(add))
            {
              ErlangClient.println(message + " added.");
            }
            else if (ref.equals(reply_ref) && reply_result.equals(remove))
            {
              ErlangClient.println(message + " removed.");
            }
            else
            {
              throw new ClassCastException();
            }
          }
          catch (final ClassCastException e)
          {
            ErlangClient.println_err("Got unexpected message: " + reply);
          }
        }
        else
        {
          ErlangClient.println_err("Got unexpected message: " + reply);
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
}