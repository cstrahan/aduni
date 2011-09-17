import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;


public class ServTest extends HttpServlet {

    public void service(HttpServletRequest request, 
			HttpServletResponse response)
			throws IOException, ServletException{
        response.setContentType("text/html");
        PrintWriter out = response.getWriter();
	HttpSession session = request.getSession();
	Integer count = (Integer)session.getAttribute("count");
	if(count == null)
	    count = new Integer(0);
	session.setAttribute("count",new Integer(count.intValue() + 1));
	out.println("<html><head><title>Test</title></head>");
	out.println("<body><h1>Hello ADU servlet</h1>");
	out.println("count = " + count);
	out.println("</body></html>");
    }
}
