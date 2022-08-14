package de.z0rdak.regionshield.handler;

import net.minecraft.command.CommandSource;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

public class CommandInterceptor {

    @SubscribeEvent
    public void handleModCommandPermission(CommandEvent event){
        CommandSource src = event.getParseResults().getContext().getSource();
        if (src.getEntity() == null) {
            // server console or command block
            // TODO: Check both and add config for command block
        } else {
            // most likely player executing command
            // TODO: handle permissions
        }
    }
}
