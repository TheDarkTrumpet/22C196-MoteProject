/**
 * 
 */
package wifilocator.gui;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Matrix;
import android.graphics.Paint;
import android.graphics.PointF;
import android.util.FloatMath;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.ImageView;

/**
 * Listener for Touch Event on the map
 * @author Eric
 * @version 0
 */
public class MapTouchListener implements OnTouchListener{

	private static int NONE=0;
	private static int DRAG=1;
	private static int ZOOM=2;
	private int mode;
	private Matrix currentMatrix;
	private Matrix savedMatrix;
	private PointF prevP;
	private PointF midP;
	private float dist;
	private ImageView upperLayer;
	private ImageView bottomLayer;
	private int level;
	//test for GUI
	
	/**
	 * Constructor function
	 * @author Eric Wang
	 * @param map_image
	 * @param user_image
	 * @param indicator, to indicate this TouchListener belongs to the upper layer
	 * or the bottom layer
	 */
	public MapTouchListener(ImageView upperLayer,ImageView bottomLayer,int level)
	{
		currentMatrix=new Matrix();
		savedMatrix=new Matrix();
		prevP=new PointF();
		midP=new PointF();
		mode=NONE;
		this.upperLayer=upperLayer;
		this.bottomLayer=bottomLayer;
		this.level=level;
		if(this.level==0)
		{
			bottomLayer.setImageMatrix(currentMatrix);
		}
		else
		{
			upperLayer.setImageMatrix(currentMatrix);
		}
		//test for GUI
		//map_image.draw(canvas);
	}
	
	/**
	 * Implementation of OnTouch function in OnTouchListener interface
	 * Process Drag, and Zoom
	 * @author Eric Wang
	 */
	public boolean onTouch(View v, MotionEvent event) {
		// TODO Auto-generated method stub
		switch(event.getAction()&MotionEvent.ACTION_MASK)
		{
		case MotionEvent.ACTION_DOWN:
			prevP.set(event.getX(), event.getY());
			mode=DRAG;
			savedMatrix.set(currentMatrix);
			break;
		case MotionEvent.ACTION_POINTER_DOWN:
			dist=distance(event);
			if(dist>10f)
			{
				savedMatrix.set(currentMatrix);
				midPoint(midP, event);
				mode=ZOOM;
			}
			break;
		case MotionEvent.ACTION_UP:
		case MotionEvent.ACTION_POINTER_UP:
			mode=NONE;
			break;
		case MotionEvent.ACTION_MOVE:
			if(mode==DRAG)
			{
				currentMatrix.set(savedMatrix);
				currentMatrix.postTranslate(event.getX()-prevP.x, event.getY()-prevP.y);
			}
			else if(mode==ZOOM)
			{
				float newDist=distance(event);
				if(newDist>10f)
				{
					currentMatrix.set(savedMatrix);
					float tScale=newDist/dist;
					currentMatrix.postScale(tScale, tScale, midP.x, midP.y);
				}
			}
			break;
		}
		if(level==0)
		{
			bottomLayer.setImageMatrix(currentMatrix);
		}
		else 
		{
			upperLayer.setImageMatrix(currentMatrix);
			//pass the Touch Event from the upperLayer to bottom Layer
			bottomLayer.dispatchTouchEvent(event);
		}
		return true;
	}
	
	/**
	 * Get the distance between 2 points in a muli-Touch motion Event
	 * @author Eric Wang
	 * @param event MotionEvent 
	 * @return distance between 2 points
	 */
	public float distance(MotionEvent event)
	{
		float x =event.getX(0) - event.getX(1);
		float y =event.getY(0) - event.getY(1);
		return FloatMath.sqrt(x*x+y*y);
	}
	
	/**
	 * Get the mid Point between 2 points in a multi-Touch motion Event
	 * @author Eric Wang
	 * @param point result middle point is stored here
	 * @param event MotionEvent
	 */
	private void midPoint(PointF point, MotionEvent event) {
        float x = event.getX(0) + event.getX(1);
        float y = event.getY(0) + event.getY(1);
        point.set(x/2, y/2);
    }
	
	/**
	 * 
	 */
//	private void minZoom() {
//        float minScaleR = Math.min(
//                (float) map_image.getWidth() / (float) map.getWidth(),
//                (float) map_image.getHeight() / (float) map.getHeight());
//        if (minScaleR < 1.0) {
//            currentMatrix.postScale(minScaleR, minScaleR);
//        }
//    }
	
}
