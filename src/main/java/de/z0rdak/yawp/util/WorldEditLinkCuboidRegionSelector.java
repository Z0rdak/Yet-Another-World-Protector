package de.z0rdak.yawp.util;

import com.sk89q.worldedit.regions.selector.CuboidRegionSelector;
import com.sk89q.worldedit.regions.selector.limit.SelectorLimits;
import com.sk89q.worldedit.world.World;
import com.sk89q.worldedit.entity.Player;
import com.sk89q.worldedit.math.BlockVector3;

import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.Text;

import static com.google.common.base.Preconditions.checkNotNull;
import javax.annotation.Nullable;


public class WorldEditLinkCuboidRegionSelector extends CuboidRegionSelector {

	protected ServerCommandSource src;
	protected IMarkableRegion region;
	
    /**
     * Create a new region selector with the given two positions.
     *
     * @param world the world
     * @param position1 position 1
     * @param position2 position 2
     */
    public WorldEditLinkCuboidRegionSelector(ServerCommandSource src, IMarkableRegion region, @Nullable World world, BlockVector3 position1, BlockVector3 position2) {
        super(world, position1, position2);
        this.src = src;
        this.region = region;
    }
    
    protected void maybeSendToYapw() {
    	if (src==null || region == null) {
    		return;
    	}
		WorldEditLinkUtil.worldEditToRegion(src, region);    	
    }
    
    @Override
    public void learnChanges() {
    	super.learnChanges();
    	maybeSendToYapw();
    }
    
    @Override
    public void clear() {
    	// Break the link between the worldedit region and the yawp region
    	src = null;
    	region = null;
    	super.clear();
    }
    
    @Override
    public boolean selectPrimary(BlockVector3 position, SelectorLimits limits) {
    	boolean ret = super.selectPrimary(position, limits);
    	maybeSendToYapw();
    	return ret;
    }

    @Override
    public boolean selectSecondary(BlockVector3 position, SelectorLimits limits) {
    	boolean ret = super.selectSecondary(position, limits);
    	maybeSendToYapw();
    	return ret;
    }

}

